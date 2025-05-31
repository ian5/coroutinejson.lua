--
-- json.lua
--
-- Copyright (c) 2020 rxi
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy of
-- this software and associated documentation files (the "Software"), to deal in
-- the Software without restriction, including without limitation the rights to
-- use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is furnished to do
-- so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--

local json = { _version = "0.1.2" }

json.unfinished = {}

-------------------------------------------------------------------------------
-- Encode
-------------------------------------------------------------------------------

local encode

-- This table is both really funny looking and maps certain special characters to their escape sequence
local escape_char_map = {
  [ "\\" ] = "\\",
  [ "\"" ] = "\"",
  [ "\b" ] = "b",
  [ "\f" ] = "f",
  [ "\n" ] = "n",
  [ "\r" ] = "r",
  [ "\t" ] = "t",
}

-- This table inverts the escape table, thus mapping escape sequences to their special character
local escape_char_map_inv = { [ "/" ] = "/" }
for k, v in pairs(escape_char_map) do
  escape_char_map_inv[v] = k
end

-- Return an escape sequence for the given character
local function escape_char(c)
  -- If it's a character with a special escape sequence, use that
  return "\\" .. (escape_char_map[c] or 
  -- Otherwise, we have to encode it by character code
  string.format("u%04x", c:byte()))
end

-- Encode nil as a null value
local function encode_nil(val)
  return "null"
end


local function encode_table(val, stack)
  local res = {}
  stack = stack or {}

  -- Circular reference?
  if stack[val] then error("circular reference") end

  stack[val] = true

  if rawget(val, 1) ~= nil or next(val) == nil then
    -- Treat as array -- check keys are valid and it is not sparse
    local n = 0
    for k in pairs(val) do
      if type(k) ~= "number" then
        error("invalid table: mixed or invalid key types")
      end
      n = n + 1
    end
    if n ~= #val then
      error("invalid table: sparse array")
    end
    -- Encode
    for i, v in ipairs(val) do
      table.insert(res, encode(v, stack))
    end
    stack[val] = nil
    return "[" .. table.concat(res, ",") .. "]"

  else
    -- Treat as an object
    for k, v in pairs(val) do
      if type(k) ~= "string" then
        error("invalid table: mixed or invalid key types")
      end
      table.insert(res, encode(k, stack) .. ":" .. encode(v, stack))
    end
    stack[val] = nil
    return "{" .. table.concat(res, ",") .. "}"
  end
end


local function encode_string(val)
  return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"'
end


local function encode_number(val)
  -- Check for NaN, -inf and inf
  if val ~= val or val <= -math.huge or val >= math.huge then
    error("unexpected number value '" .. tostring(val) .. "'")
  end
  return string.format("%.14g", val)
end


local type_func_map = {
  [ "nil"     ] = encode_nil,
  [ "table"   ] = encode_table,
  [ "string"  ] = encode_string,
  [ "number"  ] = encode_number,
  [ "boolean" ] = tostring,
}

-- Encode a specific value 
encode = function(val, stack)
  local t = type(val)
  local f = type_func_map[t]
  if f then
    return f(val, stack)
  end
  error("unexpected type '" .. t .. "'")
end

-- Encode a lua value as a JSON string and return it
function json.encode(val)
  -- This function is only a wrapper, so that the user can't mess with the recursive encode function directly
  return ( encode(val) )
end


-------------------------------------------------------------------------------
-- Decode
-------------------------------------------------------------------------------

local parse

-- helper function to create the character sets
local function create_set(...)
  local res = {}
  for i = 1, select("#", ...) do
    res[ select(i, ...) ] = true
  end
  return res
end

local space_chars   = create_set(" ", "\t", "\r", "\n")
local delim_chars   = create_set(" ", "\t", "\r", "\n", "]", "}", ",")
local escape_chars  = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u")
local literals      = create_set("true", "false", "null")

local literal_map = {
  [ "true"  ] = true,
  [ "false" ] = false,
  [ "null"  ] = nil,
}

-- Find the first occurance in str of a character from set after idx; if negated, look for any character *not* in set
local function next_char(str, idx, set, negate)
  -- For each position from the starting index to the end of the string
  for i = idx, #str do
    -- If we find the character we're looking for
    if set[str:sub(i, i)] ~= negate then
      -- Return its index
      return i
    end
  end
  -- Otherwise, return one more than the end of the string
  return #str + 1
end


local function decode_error(str, idx, msg)
  local line_count = 1
  local col_count = 1
  for i = 1, idx - 1 do
    col_count = col_count + 1
    if str:sub(i, i) == "\n" then
      line_count = line_count + 1
      col_count = 1
    end
  end
  error( string.format("%s at line %d col %d", msg, line_count, col_count) )
end


local function codepoint_to_utf8(n)
  -- http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=iws-appendixa
  local f = math.floor
  if n <= 0x7f then
    return string.char(n)
  elseif n <= 0x7ff then
    return string.char(f(n / 64) + 192, n % 64 + 128)
  elseif n <= 0xffff then
    return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128, n % 64 + 128)
  elseif n <= 0x10ffff then
    return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,
                       f(n % 4096 / 64) + 128, n % 64 + 128)
  end
  error( string.format("invalid unicode codepoint '%x'", n) )
end


local function parse_unicode_escape(s)
  local n1 = tonumber( s:sub(1, 4),  16 )
  local n2 = tonumber( s:sub(7, 10), 16 )
   -- Surrogate pair?
  if n2 then
    return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
  else
    return codepoint_to_utf8(n1)
  end
end

-- TODO: refactor this to use table.concat instead of repeated concatenation operations
local function parse_string(str, index)
  local resolved = ""
  local chunk_end = index + 1
  local chunk_start = chunk_end

  -- Until we're finished processing the string
  while chunk_end <= #str do
    -- Check the next character of the string as a number
    local char = str:byte(chunk_end)

    -- Error if it's a control code; json forbids control characters within strings
    if char < 32 then
      decode_error(str, chunk_end, "control character in string")

    -- If we're looking at an escape sequence
    elseif char == 92 then -- `\`: Escape
      -- Append the last chunk to the final string
      resolved = resolved .. str:sub(chunk_start, chunk_end - 1)
      
      -- Set the end of the chunk to the next character
      chunk_end = chunk_end + 1
      -- And get that character
      local escape = str:sub(chunk_end, chunk_end)
      -- If this is a hex escape string...
      if escape == "u" then
        -- Get the contents of the hex code
        local hex = str:match("^[dD][89aAbB]%x%x\\u%x%x%x%x", chunk_end + 1)
                 or str:match("^%x%x%x%x", chunk_end + 1)
                 or decode_error(str, chunk_end - 1, "invalid unicode escape in string")
        -- And append that character to the final string
        resolved = resolved .. parse_unicode_escape(hex)
        -- Then move the right side of the chunk past the escape sequence
        chunk_end = chunk_end + #hex
      -- If this is not a hex escape string...
      else
        -- Check if it's one of the special escapes
        if not escape_chars[escape] then
          decode_error(str, chunk_end - 1, "invalid escape char '" .. escape .. "' in string")
        end
        -- And if it is, append that to the final string 
        resolved = resolved .. escape_char_map_inv[escape]
      end
      -- Regardless, the start of the next chunk is after the final string
      chunk_start = chunk_end + 1
    -- If this is the end of the string
    elseif char == 34 then -- `"`: End of string
      -- Add the last chunk to the final string
      resolved = resolved .. str:sub(chunk_start, chunk_end - 1)
      -- Return the final string, and the index the parent function should resume from (immediately after the end of this string)
      return resolved, chunk_end + 1
    end
    -- Move the right side of the current chunk to the next character
    chunk_end = chunk_end + 1
  end

  decode_error(str, i, "expected closing quote for string")
end


local function parse_number(str, index)
  -- Find the last string index that's part of this number
  local last_index = next_char(str, index, delim_chars)
  -- Get the string representing the number
  local number_string = str:sub(index, last_index - 1)
  -- Convert it to a number (by lua's native format!)
  local num = tonumber(number_string)
  if not num then
    decode_error(str, index, "invalid number '" .. s .. "'")
  end
  -- Give back the number, and the index the parent function should resume from (immediately after the end of this string)
  return num, last_index
end


local function parse_literal(str, index)
  -- Find the end of the literal
  local last_index = next_char(str, index, delim_chars)
  -- Use that to get the string representing the literal
  local word = str:sub(index, last_index - 1)
  -- Fail if the literal doesn't exist
  if not literals[word] then
    decode_error(str, index, "invalid literal '" .. word .. "'")
  end
  -- Return the literal, and the index the parent function should resume from (immediately after the literal)
  return literal_map[word], last_index
end


local function parse_array(str, index)
  -- We mark the array as incomplete with the marker
  local resolved = {[json.unfinished] = true}
  -- And we first try to process item 1 of the array
  local array_index = 1
  -- And the character *after* the opening [ 
  index = index + 1
  -- Until we're done with the array
  while 1 do
    -- Yield once per item in the array
    coroutine.yield(resolved)
    -- Find the next non whitespace character
    index = next_char(str, index, space_chars, true)
    -- If it's the end of the array, we're done here
    if str:sub(index, index) == "]" then
      -- The last index is *after* the end of the array 
      index = index + 1
      break
    end
    -- Read token
    local value
    -- Get the value and end point of the array item
    value, index = parse(str, index)
    -- Add that item to the final array
    resolved[array_index] = value
    -- Move on to the next index
    array_index = array_index + 1
    -- Find the index of the next non whitespace character
    index = next_char(str, index, space_chars, true)
    -- And find that character
    local chr = str:sub(index, index)
    index = index + 1
    -- If it's the end of the array we're done 
    if chr == "]" then break end
    -- If it's neither that nor the next item, it's malformed
    if chr ~= "," then decode_error(str, index, "expected ']' or ','") end
  end
  -- The array is done; ergo it is not unfinished anymore
  resolved[json.unfinished] = nil
  return resolved, index
end


local function parse_object(str, index)
  -- The table isn't finished yet when we create it 
  local resolved = {[json.unfinished] = true}
  -- Start from the character after the opening {
  index = index + 1
  -- Until we're done 
  while 1 do
    -- Yield once per object property
    coroutine.yield(resolved)
    local key, val
    -- Find the next non whitespace character
    index = next_char(str, index, space_chars, true)
    -- If it's the end of the object
    if str:sub(index, index) == "}" then
      -- Put the index right after the object
      index = index + 1
      -- And we're done
      break
    end
    -- Unless it's not the start of a key
    if str:sub(index, index) ~= '"' then
      decode_error(str, index, "expected string for key")
    end
    -- Get the string of the key, and move the index in the main string after it 
    key, index = parse(str, index)
    -- Find the next non whitespace character
    index = next_char(str, index, space_chars, true)
    -- And unless it's not the delimiter
    if str:sub(index, index) ~= ":" then
      decode_error(str, index, "expected ':' after key")
    end
    -- Go to the next non whitespace character
    index = next_char(str, index + 1, space_chars, true)
    -- And read the value
    val, index = parse(str, index)
    -- Save that pair
    resolved[key] = val
    -- Move to next token
    index = next_char(str, index, space_chars, true)
    -- Get the first character of it
    local chr = str:sub(index, index)
    -- Then move past it
    index = index + 1
    -- If it was the end of the object, we're done 
    if chr == "}" then break end
    -- If it wasn't that or a comma, it's broken
    if chr ~= "," then decode_error(str, index, "expected '}' or ','") end
  end
  -- Give back the object and the index immediately after it 
  resolved[json.unfinished] = nil
  return resolved, index
end


local char_func_map = {
  [ '"' ] = parse_string,
  [ "0" ] = parse_number,
  [ "1" ] = parse_number,
  [ "2" ] = parse_number,
  [ "3" ] = parse_number,
  [ "4" ] = parse_number,
  [ "5" ] = parse_number,
  [ "6" ] = parse_number,
  [ "7" ] = parse_number,
  [ "8" ] = parse_number,
  [ "9" ] = parse_number,
  [ "-" ] = parse_number,
  [ "t" ] = parse_literal,
  [ "f" ] = parse_literal,
  [ "n" ] = parse_literal,
  [ "[" ] = parse_array,
  [ "{" ] = parse_object,
}


parse = function(str, idx)
  -- Get the character at the index we're working with
  local chr = str:sub(idx, idx)
  -- Find the parsing function associated with it
  local f = char_func_map[chr]
  -- And call it with the input and our current working index
  if f then
    return f(str, idx, iterations)
  end
  -- If we didn't find that then it's broken
  decode_error(str, idx, "unexpected character '" .. chr .. "'")
end

local function coroutine_decode(str)
  -- This function expects a string
  if type(str) ~= "string" then
    error("expected argument of type string, got " .. type(str))
  end
  -- The parsing routine is a recursive function 
  -- Retrieve the output table and the last index of the string that was processed...
  local res, idx = 
  -- ...from the input, starting from the  first non-whitespace character
  parse(str, next_char(str, 1, space_chars, true))
  -- Run the index past any trailing whitespace
  idx = next_char(str, idx, space_chars, true)
  -- If we still haven't covered the whole string, there's trailing garbage
  if idx <= #str then
    decode_error(str, idx, "trailing garbage")
  end
  return res
end

-- Return the coroutine decoder directly
function json.coroutine_decode()
  return coroutine.create(coroutine_decode)
end

-- Automatically run through a whole string, ignoring all the coroutine mechanics
function json.decode(str)
  -- Make the coroutine for this object
  local co = json.coroutine_decode()
  local success, output
  -- Until the coroutine is done 
  repeat 
    -- Gather the informatino from the coroutine
    success, output = coroutine.resume(co, str)
    -- If there was an error, propigate it up
    if not success then error(output) end
  until coroutine.status(co) == 'dead'
  -- Once the decoder is done, give the output
  return output
end

-- This function checks if the provided table has the unfinished flag set
function json.complete(tbl)
  return not tbl[unfinished]
end

local decoder_metatable = {
  __index = {
    -- Run the decoder for a specified number of iterations. Returns the loaded object if it finishes.
    work = function(self, iterations)
      -- As many times as we were asked to
      for i=1, iterations do
        -- Run an iteration
        local complete, output = self:resume()
        -- And if we're done, return it
        if complete then return output end
      end
    -- If we haven't returned yet, the coroutine didn't finish; we don't return anything 
    end,
    -- Directly resume the decoder until it next yields. Returns whether it was completed, and the most recent output from the decoder
    resume = function(self)
      -- Gather the coroutine outputs
      local success, output = coroutine.resume(self.coroutine, self.string)
      -- If the decoder threw an error, propigate that
      if not success then error(output) end
      -- Update the contents of the decoder (although it should be pretty rare that it's not the same table? seems prudent but maybe could cut this)
      self.contents = output
      -- If the coroutine returned, then we're done decoding this table
      local complete = coroutine.status(self.coroutine) == "dead"
      return complete, output
    end,
    -- Decode the entire remainder of the table in blocking fashion
    decode = function(self)
      local complete, output 
      -- Until we're done
      repeat
        complete, output = self:resume()
      until complete
      return output
    end
  } 
}

-- Return a decoder object, which handles a single json string
function json.decoder(str)
  -- Create the table for this object
  local obj = {
    string = str,
    json = {},
    -- Create the coroutine this object wraps
    coroutine = json.coroutine_decode()
  }
  setmetatable(obj, decoder_metatable)
  return obj
end

return json
