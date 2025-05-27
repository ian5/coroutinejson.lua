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
local function parse_string(str, i)
  local resolved = ""
  local chunk_end = i + 1
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
      resolved = resolved .. str:sub(k, chunk_end - 1)
      
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
  local number_string = str:sub(index, number_end - 1)
  local num = tonumber(number_string)
  -- Convert it to a number (by lua's native format!)
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
  -- We start with no items in the array
  local resolved = {}
  -- And we first try to process item 1 of the array
  local array_index = 1
  -- And the character *after* the opening [ 
  index = index + 1
  -- Until we're done with the array
  while 1 do
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
  return resolved, index
end


local function parse_object(str, i)
  local res = {}
  i = i + 1
  while 1 do
    local key, val
    i = next_char(str, i, space_chars, true)
    -- Empty / end of object?
    if str:sub(i, i) == "}" then
      i = i + 1
      break
    end
    -- Read key
    if str:sub(i, i) ~= '"' then
      decode_error(str, i, "expected string for key")
    end
    key, i = parse(str, i)
    -- Read ':' delimiter
    i = next_char(str, i, space_chars, true)
    if str:sub(i, i) ~= ":" then
      decode_error(str, i, "expected ':' after key")
    end
    i = next_char(str, i + 1, space_chars, true)
    -- Read value
    val, i = parse(str, i)
    -- Set
    res[key] = val
    -- Next token
    i = next_char(str, i, space_chars, true)
    local chr = str:sub(i, i)
    i = i + 1
    if chr == "}" then break end
    if chr ~= "," then decode_error(str, i, "expected '}' or ','") end
  end
  return res, i
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
    return f(str, idx)
  end
  -- If we didn't find that then it's broken
  decode_error(str, idx, "unexpected character '" .. chr .. "'")
end


function json.decode(str)
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


return json
