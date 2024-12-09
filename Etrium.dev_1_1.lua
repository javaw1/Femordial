_G._DEBUG = false

local json = (function() 
    local json = { _version = "0.1.2" }  local encode local escape_char_map = {   [ "\\" ] = "\\",   [ "\"" ] = "\"",   [ "\b" ] = "b",   [ "\f" ] = "f",   [ "\n" ] = "n",   [ "\r" ] = "r",   [ "\t" ] = "t", }  local escape_char_map_inv = { [ "/" ] = "/" } for k, v in pairs(escape_char_map) do   escape_char_map_inv[v] = k end  local function escape_char(c)   return "\\" .. (escape_char_map[c] or string.format("u%04x", c:byte())) end  local function encode_nil(val)   return "null" end  local function encode_table(val, stack)   local res = {}   stack = stack or {} if stack[val] then error("circular reference") end stack[val] = true if rawget(val, 1) ~= nil or next(val) == nil then  local n = 0  for k in pairs(val) do    if type(k) ~= "number" then   error("invalid table: mixed or invalid key types")    end    n = n + 1  end  if n ~= #val then    error("invalid table: sparse array")  end  for i, v in ipairs(val) do    table.insert(res, encode(v, stack))  end  stack[val] = nil  return "[" .. table.concat(res, ",") .. "]" else  for k, v in pairs(val) do    if type(k) ~= "string" then   error("invalid table: mixed or invalid key types")    end    table.insert(res, encode(k, stack) .. ":" .. encode(v, stack))  end  stack[val] = nil  return "{" .. table.concat(res, ",") .. "}"   end end  local function encode_string(val)   return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"' end  local function encode_number(val)   if val ~= val or val <= -math.huge or val >= math.huge then  error("unexpected number value '" .. tostring(val) .. "'")   end   return string.format("%.14g", val) end  local type_func_map = {   [ "nil"  ] = encode_nil,   [ "table"   ] = encode_table,   [ "string"  ] = encode_string,   [ "number"  ] = encode_number,   [ "boolean" ] = tostring, }  encode = function(val, stack)   local t = type(val)   local f = type_func_map[t]   if f then  return f(val, stack)   end   error("unexpected type '" .. t .. "'") end  function json.encode(val)   return ( encode(val) ) end  local parse local function create_set(...)   local res = {}   for i = 1, select("#", ...) do  res[ select(i, ...) ] = true   end   return res end  local space_chars   = create_set(" ", "\t", "\r", "\n") local delim_chars   = create_set(" ", "\t", "\r", "\n", "]", "}", ",") local escape_chars  = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u") local literals   = create_set("true", "false", "null")  local literal_map = {   [ "true"  ] = true,   [ "false" ] = false,   [ "null"  ] = nil, }  local function next_char(str, idx, set, negate)   for i = idx, #str do  if set[str:sub(i, i)] ~= negate then    return i  end   end   return #str + 1 end  local function decode_error(str, idx, msg)   local line_count = 1   local col_count = 1   for i = 1, idx - 1 do  col_count = col_count + 1  if str:sub(i, i) == "\n" then    line_count = line_count + 1    col_count = 1  end   end   error( string.format("%s at line %d col %d", msg, line_count, col_count) ) end  local function codepoint_to_utf8(n)   local f = math.floor   if n <= 0x7f then  return string.char(n)   elseif n <= 0x7ff then  return string.char(f(n / 64) + 192, n % 64 + 128)   elseif n <= 0xffff then  return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128, n % 64 + 128)   elseif n <= 0x10ffff then  return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,      f(n % 4096 / 64) + 128, n % 64 + 128)   end   error( string.format("invalid unicode codepoint '%x'", n) ) end  local function parse_unicode_escape(s)   local n1 = tonumber( s:sub(1, 4),  16 )   local n2 = tonumber( s:sub(7, 10), 16 )   if n2 then  return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)   else  return codepoint_to_utf8(n1)   end end  local function parse_string(str, i)   local res = ""   local j = i + 1   local k = j while j <= #str do  local x = str:byte(j)   if x < 32 then    decode_error(str, j, "control character in string")   elseif x == 92 then  res = res .. str:sub(k, j - 1)    j = j + 1    local c = str:sub(j, j)    if c == "u" then   local hex = str:match("^[dD][89aAbB]%x%x\\u%x%x%x%x", j + 1)      or str:match("^%x%x%x%x", j + 1)      or decode_error(str, j - 1, "invalid unicode escape in string")   res = res .. parse_unicode_escape(hex)   j = j + #hex    else   if not escape_chars[c] then     decode_error(str, j - 1, "invalid escape char '" .. c .. "' in string")   end   res = res .. escape_char_map_inv[c]    end    k = j + 1   elseif x == 34 then  res = res .. str:sub(k, j - 1)    return res, j + 1  end   j = j + 1   end decode_error(str, i, "expected closing quote for string") end  local function parse_number(str, i)   local x = next_char(str, i, delim_chars)   local s = str:sub(i, x - 1)   local n = tonumber(s)   if not n then  decode_error(str, i, "invalid number '" .. s .. "'")   end   return n, x end  local function parse_literal(str, i)   local x = next_char(str, i, delim_chars)   local word = str:sub(i, x - 1)   if not literals[word] then  decode_error(str, i, "invalid literal '" .. word .. "'")   end   return literal_map[word], x end  local function parse_array(str, i)   local res = {}   local n = 1   i = i + 1   while 1 do  local x  i = next_char(str, i, space_chars, true)  if str:sub(i, i) == "]" then    i = i + 1    break  end  x, i = parse(str, i)  res[n] = x  n = n + 1  i = next_char(str, i, space_chars, true)  local chr = str:sub(i, i)  i = i + 1  if chr == "]" then break end  if chr ~= "," then decode_error(str, i, "expected ']' or ','") end   end   return res, i end  local function parse_object(str, i)   local res = {}   i = i + 1   while 1 do  local key, val  i = next_char(str, i, space_chars, true)  if str:sub(i, i) == "}" then    i = i + 1    break  end  if str:sub(i, i) ~= '"' then    decode_error(str, i, "expected string for key")  end  key, i = parse(str, i)  i = next_char(str, i, space_chars, true)  if str:sub(i, i) ~= ":" then    decode_error(str, i, "expected ':' after key")  end  i = next_char(str, i + 1, space_chars, true)  val, i = parse(str, i)  res[key] = val  i = next_char(str, i, space_chars, true)  local chr = str:sub(i, i)  i = i + 1  if chr == "}" then break end  if chr ~= "," then decode_error(str, i, "expected '}' or ','") end   end   return res, i end  local char_func_map = {   [ '"' ] = parse_string,   [ "0" ] = parse_number,   [ "1" ] = parse_number,   [ "2" ] = parse_number,   [ "3" ] = parse_number,   [ "4" ] = parse_number,   [ "5" ] = parse_number,   [ "6" ] = parse_number,   [ "7" ] = parse_number,   [ "8" ] = parse_number,   [ "9" ] = parse_number,   [ "-" ] = parse_number,   [ "t" ] = parse_literal,   [ "f" ] = parse_literal,   [ "n" ] = parse_literal,   [ "[" ] = parse_array,   [ "{" ] = parse_object, }  parse = function(str, idx)   local chr = str:sub(idx, idx)   local f = char_func_map[chr]   if f then  return f(str, idx)   end   decode_error(str, idx, "unexpected character '" .. chr .. "'") end  function json.parse(str)   if type(str) ~= "string" then  error("expected argument of type string, got " .. type(str))   end   local res, idx = parse(str, next_char(str, 1, space_chars, true))   idx = next_char(str, idx, space_chars, true)   if idx <= #str then  decode_error(str, idx, "trailing garbage")   end   return res end  return json 
end)()

local panorama = (function() 
    local _INFO, cast, typeof, new, find_pattern, create_interface, api, safe_mode, _error, exception, exceptionCb, rawgetImpl, rawsetImpl, __thiscall, table_copy, vtable_bind, interface_ptr, vtable_entry, vtable_thunk, proc_bind, follow_call, v8js_args, v8js_function, is_array, nullptr, intbuf, panorama, vtable, DllImport, UIEngine, nativeIsValidPanelPointer, nativeGetLastDispatchedEventTargetPanel, nativeCompileRunScript, nativeRunScript, nativeGetV8GlobalContext, nativeGetIsolate, nativeGetParent, nativeGetID, nativeFindChildTraverse, nativeGetJavaScriptContextParent, nativeGetPanelContext, jsContexts, getJavaScriptContextParent, v8_dll, persistentTbl, Local, MaybeLocal, PersistentProxy_mt, Persistent, Value, Object, Array, Function, ObjectTemplate, FunctionTemplate, FunctionCallbackInfo, Primitive, Null, Undefined, Boolean, Number, Integer, String, Isolate, Context, HandleScope, TryCatch, Script, PanelInfo_t, CUtlVector_Constructor_t, panelList, panelArrayOffset, panelArray _INFO = { _VERSION = 1.3 } setmetatable(_INFO, { __call = function(self) return self._VERSION end, __tostring = function(self) return self._VERSION end }) if _G and not ffi then ffi = require("ffi") end do local _obj_0 = ffi cast, typeof, new = _obj_0.cast, _obj_0.typeof, _obj_0.new end find_pattern = function() return error("Unsupported provider (e.g. gamesense, neverlose)") end create_interface = function() return error("Unsupported provider (e.g. gamesense, neverlose)") end api = (_G == nil) and (info.fatality == nil and "ev0lve" or "fa7ality") or (file == nil and (GameEventManager == nil and (penetration == nil and (math_utils == nil and "primordial" or "legion") or "pandora") or "memesense") or "legendware") local _exp_0 = api if "ev0lve" == _exp_0 then find_pattern = utils.find_pattern create_interface = utils.find_interface elseif "fa7ality" == _exp_0 then find_pattern = utils.find_pattern create_interface = utils.find_interface elseif "primordial" == _exp_0 then find_pattern = memory.find_pattern create_interface = memory.create_interface elseif "memesense" == _exp_0 then find_pattern = Utils.PatternScan create_interface = Utils.CreateInterface elseif "legendware" == _exp_0 then find_pattern = utils.find_signature create_interface = utils.create_interface elseif "pandora" == _exp_0 then find_pattern = client.find_sig create_interface = client.create_interface elseif "legion" == _exp_0 then find_pattern = memory.find_pattern create_interface = memory.create_interface end safe_mode = xpcall and true or false _error = error if 1 + 2 == 3 then error = function(msg) for _, v in pairs(persistentTbl) do Persistent(v):disposeGlobal() end return _error(msg) end end exception = function(msg) return print("Caught exception in V8 HandleScope: ", tostring(msg)) end exceptionCb = function(msg) return print("Caught exception in V8 Function Callback: ", tostring(msg)) end rawgetImpl = function(tbl, key) local mtb = getmetatable(tbl) setmetatable(tbl, nil) local res = tbl[key] setmetatable(tbl, mtb) return res end rawsetImpl = function(tbl, key, value) local mtb = getmetatable(tbl) setmetatable(tbl, nil) tbl[key] = value return setmetatable(tbl, mtb) end if not rawget then rawget = rawgetImpl end if not rawset then rawset = rawsetImpl end __thiscall = function(func, this) return function(...) return func(this, ...) end end table_copy = function(t) local _tbl_0 = { } for k, v in pairs(t) do _tbl_0[k] = v end return _tbl_0 end vtable_bind = function(module, interface, index, typedef) local addr = cast("void***", create_interface(module, interface)) or error(interface .. " is nil.") return __thiscall(cast(typedef, addr[0][index]), addr) end interface_ptr = typeof("void***") vtable_entry = function(instance, i, ct) return cast(ct, cast(interface_ptr, instance)[0][i]) end vtable_thunk = function(i, ct) local t = typeof(ct) return function(instance, ...) return vtable_entry(instance, i, t)(instance, ...) end end proc_bind = (function() local fnGetProcAddress fnGetProcAddress = function() return error("Failed to load GetProcAddress") end local fnGetModuleHandle fnGetModuleHandle = function() return error("Failed to load GetModuleHandleA") end if ffi.C then ffi.cdef([[            uint32_t GetProcAddress(uint32_t, const char*); uint32_t GetModuleHandleA(const char*); ]]) fnGetProcAddress = ffi.C.GetProcAddress fnGetModuleHandle = ffi.C.GetModuleHandleA else fnGetProcAddress = cast("uint32_t(__stdcall*)(uint32_t, const char*)", cast("uint32_t**", cast("uint32_t", find_pattern("engine.dll", "FF 15 ? ? ? ? A3 ? ? ? ? EB 05")) + 2)[0][0]) fnGetModuleHandle = cast("uint32_t(__stdcall*)(const char*)", cast("uint32_t**", cast("uint32_t", find_pattern("engine.dll", "FF 15 ? ? ? ? 85 C0 74 0B")) + 2)[0][0]) end return function(module_name, function_name, typedef) return cast(typeof(typedef), fnGetProcAddress(fnGetModuleHandle(module_name), function_name)) end end)() follow_call = function(ptr) local insn = cast("uint8_t*", ptr) local _exp_1 = insn[0] if (0xE8 or 0xE9) == _exp_1 then return cast("uint32_t", insn + cast("int32_t*", insn + 1)[0] + 5) elseif 0xFF == _exp_1 then if insn[1] == 0x15 then return cast("uint32_t**", cast("const char*", ptr) + 2)[0][0] end else return ptr end end v8js_args = function(...) local argTbl = { ... } local iArgc = #argTbl local pArgv = new(("void*[%.f]"):format(iArgc)) for i = 1, iArgc do pArgv[i - 1] = Value:fromLua(argTbl[i]):getInternal() end return iArgc, pArgv end v8js_function = function(callbackFunction) return function(callbackInfo) callbackInfo = FunctionCallbackInfo(callbackInfo) local argTbl = { } local length = callbackInfo:length() if length > 0 then for i = 0, length - 1 do table.insert(argTbl, callbackInfo:get(i)) end end local val = nil if safe_mode then local status, ret = xpcall((function() return callbackFunction(unpack(argTbl)) end), exceptionCb) if status then val = ret end else val = callbackFunction(unpack(argTbl)) end return callbackInfo:setReturnValue(Value:fromLua(val):getInternal()) end end is_array = function(val) local i = 1 for _ in pairs(val) do if val[i] ~= nil then i = i + 1 else return false end end return i ~= 1 end nullptr = new("void*") intbuf = new("int[1]") panorama = { panelIDs = { } } do local _class_0 local _base_0 = { get = function(self, index, t) return __thiscall(cast(t, self.this[0][index]), self.this) end, getInstance = function(self) return self.this end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self, ptr) self.this = cast("void***", ptr) end, __base = _base_0, __name = "vtable" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 vtable = _class_0 end do local _class_0 local _base_0 = { cache = { }, get = function(self, method, typedef) if not (self.cache[method]) then self.cache[method] = proc_bind(self.file, method, typedef) end return self.cache[method] end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self, filename) self.file = filename end, __base = _base_0, __name = "DllImport" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 DllImport = _class_0 end UIEngine = vtable(vtable_bind("panorama.dll", "PanoramaUIEngine001", 11, "void*(__thiscall*)(void*)")()) nativeIsValidPanelPointer = UIEngine:get(36, "bool(__thiscall*)(void*,void const*)") nativeGetLastDispatchedEventTargetPanel = UIEngine:get(56, "void*(__thiscall*)(void*)") nativeCompileRunScript = UIEngine:get(113, "void****(__thiscall*)(void*,void*,char const*,char const*,int,int,bool)") nativeRunScript = __thiscall(cast(typeof("void*(__thiscall*)(void*,void*,void*,void*,int,bool)"), follow_call(find_pattern("panorama.dll", api == "legendware" and "E8 ? ? ? ? 8B 4C 24 10 FF 15 ?" or "E8 ? ? ? ? 8B 4C 24 10 FF 15 ? ? ? ?"))), UIEngine:getInstance()) nativeGetV8GlobalContext = UIEngine:get(123, "void*(__thiscall*)(void*)") nativeGetIsolate = UIEngine:get(129, "void*(__thiscall*)(void*)") nativeGetParent = vtable_thunk(25, "void*(__thiscall*)(void*)") nativeGetID = vtable_thunk(9, "const char*(__thiscall*)(void*)") nativeFindChildTraverse = vtable_thunk(40, "void*(__thiscall*)(void*,const char*)") nativeGetJavaScriptContextParent = vtable_thunk(218, "void*(__thiscall*)(void*)") nativeGetPanelContext = __thiscall(cast("void***(__thiscall*)(void*,void*)", follow_call(find_pattern("panorama.dll", "E8 ? ? ? ? 8B 00 85 C0 75 1B"))), UIEngine:getInstance()) jsContexts = { } getJavaScriptContextParent = function(panel) if jsContexts[panel] ~= nil then return jsContexts[panel] end jsContexts[panel] = nativeGetJavaScriptContextParent(panel) return jsContexts[panel] end v8_dll = DllImport("v8.dll") persistentTbl = { } do local _class_0 local _base_0 = { getInternal = function(self) return self.this end, globalize = function(self) local pPersistent = v8_dll:get("?GlobalizeReference@V8@v8@@CAPAPAVObject@internal@2@PAVIsolate@42@PAPAV342@@Z", "void*(__cdecl*)(void*,void*)")(nativeGetIsolate(), self.this[0]) local persistent = Persistent(pPersistent) persistentTbl[persistent:getIdentityHash()] = pPersistent return persistent end, __call = function(self) return Value(self.this[0]) end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self, val) self.this = cast("void**", val) end, __base = _base_0, __name = "Local" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 Local = _class_0 end do local _class_0 local _base_0 = { getInternal = function(self) return self.this end, toLocalChecked = function(self) if not (self.this[0] == nullptr) then return Local(self.this) end end, toValueChecked = function(self) if not (self.this[0] == nullptr) then return Value(self.this[0]) end end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self, val) self.this = cast("void**", val) end, __base = _base_0, __name = "MaybeLocal" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 MaybeLocal = _class_0 end PersistentProxy_mt = { __index = function(self, key) local this = rawget(self, "this") local ret = HandleScope()(function() return this:getAsValue():toObject():get(Value:fromLua(key):getInternal()):toValueChecked():toLua() end) if type(ret) == "table" then rawset(ret, "parent", this) end return ret end, __newindex = function(self, key, value) local this = rawget(self, "this") return HandleScope()(function() return this:getAsValue():toObject():set(Value:fromLua(key):getInternal(), Value:fromLua(value):getInternal()):toValueChecked():toLua() end) end, __len = function(self) local this = rawget(self, "this") local ret = 0 if this.baseType == "Array" then ret = HandleScope()(function() return this:getAsValue():toArray():length() end) elseif this.baseType == "Object" then ret = HandleScope()(function() return this:getAsValue():toObject():getPropertyNames():toValueChecked():toArray():length() end) end return ret end, __pairs = function(self) local this = rawget(self, "this") local ret ret = function() return nil end if this.baseType == "Object" then HandleScope()(function() local keys = Array(this:getAsValue():toObject():getPropertyNames():toValueChecked()) local current, size = 0, keys:length() ret = function() current = current + 1 local key = keys[current - 1] if current <= size then return key, self[key] end end end) end return ret end, __ipairs = function(self) local this = rawget(self, "this") local ret ret = function() return nil end if this.baseType == "Array" then HandleScope()(function() local current, size = 0, this:getAsValue():toArray():length() ret = function() current = current + 1 if current <= size then return current, self[current - 1] end end end) end return ret end, __call = function(self, ...) local this = rawget(self, "this") local args = { ... } if this.baseType ~= "Function" then error("Attempted to call file_system non-function value: " .. this.baseType) end return HandleScope()(function() local rawReturn = this:getAsValue():toFunction():setParent(rawget(self, "parent"))(unpack(args)):toLocalChecked() if rawReturn == nil then return nil else return rawReturn():toLua() end end) end, __tostring = function(self) local this = rawget(self, "this") return HandleScope()(function() return this:getAsValue():stringValue() end) end, __gc = function(self) local this = rawget(self, "this") return this:disposeGlobal() end } do local _class_0 local _base_0 = { setType = function(self, val) self.baseType = val return self end, getInternal = function(self) return self.this end, disposeGlobal = function(self) return v8_dll:get("?DisposeGlobal@V8@v8@@CAXPAPAVObject@internal@2@@Z", "void(__cdecl*)(void*)")(self.this) end, get = function(self) return MaybeLocal(HandleScope:createHandle(self.this)) end, getAsValue = function(self) return Value(HandleScope:createHandle(self.this)[0]) end, toLua = function(self) return self:get():toValueChecked():toLua() end, getIdentityHash = function(self) return v8_dll:get("?GetIdentityHash@Object@v8@@QAEHXZ", "int(__thiscall*)(void*)")(self.this) end, __call = function(self) return setmetatable({ this = self, parent = nil }, PersistentProxy_mt) end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self, val, baseType) if baseType == nil then baseType = "Value" end self.this = val self.baseType = baseType end, __base = _base_0, __name = "Persistent" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 Persistent = _class_0 end do local _class_0 local _base_0 = { fromLua = function(self, val) if val == nil then return Null(nativeGetIsolate()):getValue() end local valType = type(val) local _exp_1 = valType if "boolean" == _exp_1 then return Boolean(nativeGetIsolate(), val):getValue() elseif "number" == _exp_1 then return Number(nativeGetIsolate(), val):getInstance() elseif "string" == _exp_1 then return String(nativeGetIsolate(), val):getInstance() elseif "table" == _exp_1 then if is_array(val) then return Array:fromLua(nativeGetIsolate(), val) else return Object:fromLua(nativeGetIsolate(), val) end elseif "function" == _exp_1 then return FunctionTemplate(v8js_function(val)):getFunction()() else return error("Failed to convert from lua to v8js: Unknown type") end end, isUndefined = function(self) return v8_dll:get("?IsUndefined@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, isNull = function(self) return v8_dll:get("?IsNull@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, isBoolean = function(self) return v8_dll:get("?IsBoolean@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, isBooleanObject = function(self) return v8_dll:get("?IsBooleanObject@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, isNumber = function(self) return v8_dll:get("?IsNumber@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, isNumberObject = function(self) return v8_dll:get("?IsNumberObject@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, isString = function(self) return v8_dll:get("?IsString@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, isStringObject = function(self) return v8_dll:get("?IsStringObject@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, isObject = function(self) return v8_dll:get("?IsObject@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, isArray = function(self) return v8_dll:get("?IsArray@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, isFunction = function(self) return v8_dll:get("?IsFunction@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, booleanValue = function(self) return v8_dll:get("?BooleanValue@Value@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, numberValue = function(self) return v8_dll:get("?NumberValue@Value@v8@@QBENXZ", "double(__thiscall*)(void*)")(self.this) end, stringValue = function(self) local strBuf = new('char*[2]') local val = v8_dll:get("??0Utf8Value@String@v8@@QAE@V?$Local@VValue@v8@@@2@@Z", "struct{char* str; int length;}*(__thiscall*)(void*,void*)")(strBuf, self.this) local s = ffi.string(val.str, val.length) v8_dll:get("??1Utf8Value@String@v8@@QAE@XZ", "void(__thiscall*)(void*)")(strBuf) return s end, toObject = function(self) return Object(MaybeLocal(v8_dll:get("?ToObject@Value@v8@@QBE?AV?$Local@VObject@v8@@@2@XZ", "void*(__thiscall*)(void*,void*)")(self.this, intbuf)):toValueChecked():getInternal()) end, toArray = function(self) return Array(MaybeLocal(v8_dll:get("?ToObject@Value@v8@@QBE?AV?$Local@VObject@v8@@@2@XZ", "void*(__thiscall*)(void*,void*)")(self.this, intbuf)):toValueChecked():getInternal()) end, toFunction = function(self) return Function(MaybeLocal(v8_dll:get("?ToObject@Value@v8@@QBE?AV?$Local@VObject@v8@@@2@XZ", "void*(__thiscall*)(void*,void*)")(self.this, intbuf)):toValueChecked():getInternal()) end, toLocal = function(self) return Local(new("void*[1]", self.this)) end, toLua = function(self) if self:isUndefined() or self:isNull() then return nil end if self:isBoolean() or self:isBooleanObject() then return self:booleanValue() end if self:isNumber() or self:isNumberObject() then return self:numberValue() end if self:isString() or self:isStringObject() then return self:stringValue() end if self:isObject() then if self:isArray() then return self:toArray():toLocal():globalize():setType("Array")() end if self:isFunction() then return self:toFunction():toLocal():globalize():setType("Function")() end return self:toObject():toLocal():globalize():setType("Object")() end return error("Failed to convert from v8js to lua: Unknown type") end, getInternal = function(self) return self.this end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self, val) self.this = cast("void*", val) end, __base = _base_0, __name = "Value" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 Value = _class_0 end do local _class_0 local _parent_0 = Value local _base_0 = { fromLua = function(self, isolate, val) local obj = Object(MaybeLocal(v8_dll:get("?New@Object@v8@@SA?AV?$Local@VObject@v8@@@2@PAVIsolate@2@@Z", "void*(__cdecl*)(void*,void*)")(intbuf, isolate)):toValueChecked():getInternal()) for i, v in pairs(val) do obj:set(Value:fromLua(i):getInternal(), Value:fromLua(v):getInternal()) end return obj end, get = function(self, key) return MaybeLocal(v8_dll:get("?Get@Object@v8@@QAE?AV?$Local@VValue@v8@@@2@V32@@Z", "void*(__thiscall*)(void*,void*,void*)")(self.this, intbuf, key)) end, set = function(self, key, value) return v8_dll:get("?Set@Object@v8@@QAE_NV?$Local@VValue@v8@@@2@0@Z", "bool(__thiscall*)(void*,void*,void*)")(self.this, key, value) end, getPropertyNames = function(self) return MaybeLocal(v8_dll:get("?GetPropertyNames@Object@v8@@QAE?AV?$Local@VArray@v8@@@2@XZ", "void*(__thiscall*)(void*,void*)")(self.this, intbuf)) end, callAsFunction = function(self, recv, argc, argv) return MaybeLocal(v8_dll:get("?CallAsFunction@Object@v8@@QAE?AV?$Local@VValue@v8@@@2@V32@HQAV32@@Z", "void*(__thiscall*)(void*,void*,void*,int,void*)")(self.this, intbuf, recv, argc, argv)) end, getIdentityHash = function(self) return v8_dll:get("?GetIdentityHash@Object@v8@@QAEHXZ", "int(__thiscall*)(void*)")(self.this) end } _base_0.__index = _base_0 setmetatable(_base_0, _parent_0.__base) _class_0 = setmetatable({ __init = function(self, val) self.this = val end, __base = _base_0, __name = "Object", __parent = _parent_0 }, { __index = function(cls, name) local val = rawget(_base_0, name) if val == nil then local parent = rawget(cls, "__parent") if parent then return parent[name] end else return val end end, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 if _parent_0.__inherited then _parent_0.__inherited(_parent_0, _class_0) end Object = _class_0 end do local _class_0 local _parent_0 = Object local _base_0 = { fromLua = function(self, isolate, val) local arr = Array(MaybeLocal(v8_dll:get("?New@Array@v8@@SA?AV?$Local@VArray@v8@@@2@PAVIsolate@2@H@Z", "void*(__cdecl*)(void*,void*,int)")(intbuf, isolate, #val)):toValueChecked():getInternal()) for i = 1, #val do arr:set(i - 1, Value:fromLua(val[i]):getInternal()) end return arr end, get = function(self, key) return MaybeLocal(v8_dll:get("?Get@Object@v8@@QAE?AV?$Local@VValue@v8@@@2@I@Z", "void*(__thiscall*)(void*,void*,unsigned int)")(self.this, intbuf, key)) end, set = function(self, key, value) return v8_dll:get("?Set@Object@v8@@QAE_NIV?$Local@VValue@v8@@@2@@Z", "bool(__thiscall*)(void*,unsigned int,void*)")(self.this, key, value) end, length = function(self) return v8_dll:get("?Length@Array@v8@@QBEIXZ", "uint32_t(__thiscall*)(void*)")(self.this) end } _base_0.__index = _base_0 setmetatable(_base_0, _parent_0.__base) _class_0 = setmetatable({ __init = function(self, val) self.this = val end, __base = _base_0, __name = "Array", __parent = _parent_0 }, { __index = function(cls, name) local val = rawget(_base_0, name) if val == nil then local parent = rawget(cls, "__parent") if parent then return parent[name] end else return val end end, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 if _parent_0.__inherited then _parent_0.__inherited(_parent_0, _class_0) end Array = _class_0 end do local _class_0 local _parent_0 = Object local _base_0 = { setParent = function(self, val) self.parent = val return self end, __call = function(self, ...) if self.parent == nil then return self:callAsFunction(Context(Isolate(nativeGetIsolate()):getCurrentContext()):global():toValueChecked():getInternal(), v8js_args(...)) else return self:callAsFunction(self.parent:getAsValue():getInternal(), v8js_args(...)) end end } _base_0.__index = _base_0 setmetatable(_base_0, _parent_0.__base) _class_0 = setmetatable({ __init = function(self, val, parent) self.this = val self.parent = parent end, __base = _base_0, __name = "Function", __parent = _parent_0 }, { __index = function(cls, name) local val = rawget(_base_0, name) if val == nil then local parent = rawget(cls, "__parent") if parent then return parent[name] end else return val end end, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 if _parent_0.__inherited then _parent_0.__inherited(_parent_0, _class_0) end Function = _class_0 end do local _class_0 local _base_0 = { } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self) self.this = MaybeLocal(v8_dll:get("?New@ObjectTemplate@v8@@SA?AV?$Local@VObjectTemplate@v8@@@2@XZ", "void*(__cdecl*)(void*)")(intbuf)):toLocalChecked() end, __base = _base_0, __name = "ObjectTemplate" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 ObjectTemplate = _class_0 end do local _class_0 local _base_0 = { getFunction = function(self) return MaybeLocal(v8_dll:get("?GetFunction@FunctionTemplate@v8@@QAE?AV?$Local@VFunction@v8@@@2@XZ", "void*(__thiscall*)(void*, void*)")(self:this():getInternal(), intbuf)):toLocalChecked() end, getInstance = function(self) return self:this() end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self, callback) self.this = MaybeLocal(v8_dll:get("?New@FunctionTemplate@v8@@SA?AV?$Local@VFunctionTemplate@v8@@@2@PAVIsolate@2@P6AXABV?$FunctionCallbackInfo@VValue@v8@@@2@@ZV?$Local@VValue@v8@@@2@V?$Local@VSignature@v8@@@2@HW4ConstructorBehavior@2@@Z", "void*(__cdecl*)(void*,void*,void*,void*,void*,int,int)")(intbuf, nativeGetIsolate(), cast("void(__cdecl*)(void******)", callback), new("int[1]"), new("int[1]"), 0, 0)):toLocalChecked() end, __base = _base_0, __name = "FunctionTemplate" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 FunctionTemplate = _class_0 end do local _class_0 local _base_0 = { kHolderIndex = 0, kIsolateIndex = 1, kReturnValueDefaultValueIndex = 2, kReturnValueIndex = 3, kDataIndex = 4, kCalleeIndex = 5, kContextSaveIndex = 6, kNewTargetIndex = 7, getHolder = function(self) return MaybeLocal(self:getImplicitArgs_()[self.kHolderIndex]):toLocalChecked() end, getIsolate = function(self) return Isolate(self:getImplicitArgs_()[self.kIsolateIndex][0]) end, getReturnValueDefaultValue = function(self) return Value(new("void*[1]", self:getImplicitArgs_()[self.kReturnValueDefaultValueIndex])) end, getReturnValue = function(self) return Value(new("void*[1]", self:getImplicitArgs_()[self.kReturnValueIndex])) end, setReturnValue = function(self, value) self:getImplicitArgs_()[self.kReturnValueIndex] = cast("void**", value)[0] end, getData = function(self) return MaybeLocal(self:getImplicitArgs_()[self.kDataIndex]):toLocalChecked() end, getCallee = function(self) return MaybeLocal(self:getImplicitArgs_()[self.kCalleeIndex]):toLocalChecked() end, getContextSave = function(self) return MaybeLocal(self:getImplicitArgs_()[self.kContextSaveIndex]):toLocalChecked() end, getNewTarget = function(self) return MaybeLocal(self:getImplicitArgs_()[self.kNewTargetIndex]):toLocalChecked() end, getImplicitArgs_ = function(self) return self.this[0] end, getValues_ = function(self) return self.this[1] end, getLength_ = function(self) return self.this[2] end, length = function(self) return tonumber(cast("int", self:getLength_())) end, get = function(self, i) if self:length() > i then return Value(self:getValues_() - i):toLua() else return end end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self, val) self.this = cast("void****", val) end, __base = _base_0, __name = "FunctionCallbackInfo" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 FunctionCallbackInfo = _class_0 end do local _class_0 local _parent_0 = Value local _base_0 = { getValue = function(self) return self.this end, toString = function(self) return self.this:getValue():stringValue() end } _base_0.__index = _base_0 setmetatable(_base_0, _parent_0.__base) _class_0 = setmetatable({ __init = function(self, val) self.this = val end, __base = _base_0, __name = "Primitive", __parent = _parent_0 }, { __index = function(cls, name) local val = rawget(_base_0, name) if val == nil then local parent = rawget(cls, "__parent") if parent then return parent[name] end else return val end end, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 if _parent_0.__inherited then _parent_0.__inherited(_parent_0, _class_0) end Primitive = _class_0 end do local _class_0 local _parent_0 = Primitive local _base_0 = { } _base_0.__index = _base_0 setmetatable(_base_0, _parent_0.__base) _class_0 = setmetatable({ __init = function(self, isolate) self.this = Value(cast("uintptr_t", isolate) + 0x48) end, __base = _base_0, __name = "Null", __parent = _parent_0 }, { __index = function(cls, name) local val = rawget(_base_0, name) if val == nil then local parent = rawget(cls, "__parent") if parent then return parent[name] end else return val end end, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 if _parent_0.__inherited then _parent_0.__inherited(_parent_0, _class_0) end Null = _class_0 end do local _class_0 local _parent_0 = Primitive local _base_0 = { } _base_0.__index = _base_0 setmetatable(_base_0, _parent_0.__base) _class_0 = setmetatable({ __init = function(self, isolate) self.this = Value(cast("uintptr_t", isolate) + 0x56) end, __base = _base_0, __name = "Undefined", __parent = _parent_0 }, { __index = function(cls, name) local val = rawget(_base_0, name) if val == nil then local parent = rawget(cls, "__parent") if parent then return parent[name] end else return val end end, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 if _parent_0.__inherited then _parent_0.__inherited(_parent_0, _class_0) end Undefined = _class_0 end do local _class_0 local _parent_0 = Primitive local _base_0 = { } _base_0.__index = _base_0 setmetatable(_base_0, _parent_0.__base) _class_0 = setmetatable({ __init = function(self, isolate, bool) self.this = Value(cast("uintptr_t", isolate) + ((function() if bool then return 0x4C else return 0x50 end end)())) end, __base = _base_0, __name = "Boolean", __parent = _parent_0 }, { __index = function(cls, name) local val = rawget(_base_0, name) if val == nil then local parent = rawget(cls, "__parent") if parent then return parent[name] end else return val end end, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 if _parent_0.__inherited then _parent_0.__inherited(_parent_0, _class_0) end Boolean = _class_0 end do local _class_0 local _parent_0 = Value local _base_0 = { getLocal = function(self) return self.this end, getValue = function(self) return self:getInstance():numberValue() end, getInstance = function(self) return self:this() end } _base_0.__index = _base_0 setmetatable(_base_0, _parent_0.__base) _class_0 = setmetatable({ __init = function(self, isolate, val) self.this = MaybeLocal(v8_dll:get("?New@Number@v8@@SA?AV?$Local@VNumber@v8@@@2@PAVIsolate@2@N@Z", "void*(__cdecl*)(void*,void*,double)")(intbuf, isolate, tonumber(val))):toLocalChecked() end, __base = _base_0, __name = "Number", __parent = _parent_0 }, { __index = function(cls, name) local val = rawget(_base_0, name) if val == nil then local parent = rawget(cls, "__parent") if parent then return parent[name] end else return val end end, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 if _parent_0.__inherited then _parent_0.__inherited(_parent_0, _class_0) end Number = _class_0 end do local _class_0 local _parent_0 = Number local _base_0 = { } _base_0.__index = _base_0 setmetatable(_base_0, _parent_0.__base) _class_0 = setmetatable({ __init = function(self, isolate, val) self.this = MaybeLocal(v8_dll:get("?NewFromUnsigned@Integer@v8@@SA?AV?$Local@VInteger@v8@@@2@PAVIsolate@2@I@Z", "void*(__cdecl*)(void*,void*,uint32_t)")(intbuf, isolate, tonumber(val))):toLocalChecked() end, __base = _base_0, __name = "Integer", __parent = _parent_0 }, { __index = function(cls, name) local val = rawget(_base_0, name) if val == nil then local parent = rawget(cls, "__parent") if parent then return parent[name] end else return val end end, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 if _parent_0.__inherited then _parent_0.__inherited(_parent_0, _class_0) end Integer = _class_0 end do local _class_0 local _parent_0 = Value local _base_0 = { getLocal = function(self) return self.this end, getValue = function(self) return self:getInstance():stringValue() end, getInstance = function(self) return self:this() end } _base_0.__index = _base_0 setmetatable(_base_0, _parent_0.__base) _class_0 = setmetatable({ __init = function(self, isolate, val) self.this = MaybeLocal(v8_dll:get("?NewFromUtf8@String@v8@@SA?AV?$MaybeLocal@VString@v8@@@2@PAVIsolate@2@PBDW4NewStringType@2@H@Z", "void*(__cdecl*)(void*,void*,const char*,int,int)")(intbuf, isolate, val, 0, #val)):toLocalChecked() end, __base = _base_0, __name = "String", __parent = _parent_0 }, { __index = function(cls, name) local val = rawget(_base_0, name) if val == nil then local parent = rawget(cls, "__parent") if parent then return parent[name] end else return val end end, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 if _parent_0.__inherited then _parent_0.__inherited(_parent_0, _class_0) end String = _class_0 end do local _class_0 local _base_0 = { enter = function(self) return v8_dll:get("?Enter@Isolate@v8@@QAEXXZ", "void(__thiscall*)(void*)")(self.this) end, exit = function(self) return v8_dll:get("?Exit@Isolate@v8@@QAEXXZ", "void(__thiscall*)(void*)")(self.this) end, getCurrentContext = function(self) return MaybeLocal(v8_dll:get("?GetCurrentContext@Isolate@v8@@QAE?AV?$Local@VContext@v8@@@2@XZ", "void**(__thiscall*)(void*,void*)")(self.this, intbuf)):toValueChecked():getInternal() end, getInternal = function(self) return self.this end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self, val) if val == nil then val = nativeGetIsolate() end self.this = val end, __base = _base_0, __name = "Isolate" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 Isolate = _class_0 end do local _class_0 local _base_0 = { enter = function(self) return v8_dll:get("?Enter@Context@v8@@QAEXXZ", "void(__thiscall*)(void*)")(self.this) end, exit = function(self) return v8_dll:get("?Exit@Context@v8@@QAEXXZ", "void(__thiscall*)(void*)")(self.this) end, global = function(self) return MaybeLocal(v8_dll:get("?Global@Context@v8@@QAE?AV?$Local@VObject@v8@@@2@XZ", "void*(__thiscall*)(void*,void*)")(self.this, intbuf)) end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self, val) self.this = val end, __base = _base_0, __name = "Context" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 Context = _class_0 end do local _class_0 local _base_0 = { enter = function(self) return v8_dll:get("??0HandleScope@v8@@QAE@PAVIsolate@1@@Z", "void(__thiscall*)(void*,void*)")(self.this, nativeGetIsolate()) end, exit = function(self) return v8_dll:get("??1HandleScope@v8@@QAE@XZ", "void(__thiscall*)(void*)")(self.this) end, createHandle = function(self, val) return v8_dll:get("?CreateHandle@HandleScope@v8@@KAPAPAVObject@internal@2@PAVIsolate@42@PAV342@@Z", "void**(__cdecl*)(void*,void*)")(nativeGetIsolate(), val) end, __call = function(self, func, panel) if panel == nil then panel = panorama.GetPanel("CSGOJsRegistration") end local isolate = Isolate() isolate:enter() self:enter() local ctx if panel then ctx = nativeGetPanelContext(getJavaScriptContextParent(panel))[0] else ctx = Context(isolate:getCurrentContext()):global():getInternal() end ctx = Context((function() if ctx ~= nullptr then return self:createHandle(ctx[0]) else return 0 end end)()) ctx:enter() local val = nil if safe_mode then local status, ret = xpcall(func, exception) if status then val = ret end else val = func() end ctx:exit() self:exit() isolate:exit() return val end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self) self.this = new("char[0xC]") end, __base = _base_0, __name = "HandleScope" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 HandleScope = _class_0 end do local _class_0 local _base_0 = { enter = function(self) return v8_dll:get("??0TryCatch@v8@@QAE@PAVIsolate@1@@Z", "void(__thiscall*)(void*,void*)")(self.this, nativeGetIsolate()) end, exit = function(self) return v8_dll:get("??1TryCatch@v8@@QAE@XZ", "void(__thiscall*)(void*)")(self.this) end, canContinue = function(self) return v8_dll:get("?CanContinue@TryCatch@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, hasTerminated = function(self) return v8_dll:get("?HasTerminated@TryCatch@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end, hasCaught = function(self) return v8_dll:get("?HasCaught@TryCatch@v8@@QBE_NXZ", "bool(__thiscall*)(void*)")(self.this) end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function(self) self.this = new("char[0x19]") end, __base = _base_0, __name = "TryCatch" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 TryCatch = _class_0 end do local _class_0 local _base_0 = { compile = function(self, panel, source, layout) if layout == nil then layout = "" end return __thiscall(cast("void**(__thiscall*)(void*,void*,const char*,const char*)", api == "memesense" and find_pattern("panorama.dll", "E8 ? ? ? ? 8B 4C 24 10 FF 15 ? ? ? ?") - 2816 or find_pattern("panorama.dll", "55 8B EC 83 E4 F8 83 EC 64 53 8B D9")), UIEngine:getInstance())(panel, source, layout) end, loadstring = function(self, str, panel) local isolate = Isolate(nativeGetIsolate()) local handleScope = HandleScope() local tryCatch = TryCatch() isolate:enter() handleScope:enter() local ctx if panel then ctx = nativeGetPanelContext(getJavaScriptContextParent(panel))[0] else ctx = Context(isolate:getCurrentContext()):global():getInternal() end ctx = Context((function() if ctx ~= nullptr then return handleScope:createHandle(ctx[0]) else return 0 end end)()) ctx:enter() tryCatch:enter() local compiled = MaybeLocal(self:compile(panel, str)):toLocalChecked() tryCatch:exit() local ret if not (compiled == nil) then ret = MaybeLocal(nativeRunScript(intbuf, panel, compiled():getInternal(), 0, false)):toValueChecked():toLua() end if not (((not safe_mode) or ret)) then ret = (function() return print("WARNING: Attempted to call nullptr") end) end ctx:exit() handleScope:exit() isolate:exit() return ret end } _base_0.__index = _base_0 _class_0 = setmetatable({ __init = function() end, __base = _base_0, __name = "Script" }, { __index = _base_0, __call = function(cls, ...) local _self_0 = setmetatable({}, _base_0) cls.__init(_self_0, ...) return _self_0 end }) _base_0.__class = _class_0 Script = _class_0 end PanelInfo_t = typeof([[    struct { char* pad1[0x4]; void*         m_pPanel; void* unk1; } ]]) CUtlVector_Constructor_t = typeof([[    struct { struct { $ *m_pMemory; int m_nAllocationCount; int m_nGrowSize; } m_Memory; int m_Size; $ *m_pElements; } ]], PanelInfo_t, PanelInfo_t) ffi.metatype(CUtlVector_Constructor_t, { __index = { Count = function(self) return self.m_Memory.m_nAllocationCount end, Element = function(self, i) return cast(typeof("$&", PanelInfo_t), self.m_Memory.m_pMemory[i]) end, RemoveAll = function(self) self = nil self = typeof("$[?]", CUtlVector_Constructor_t)(1)[0] self.m_Size = 0 end }, __ipairs = function(self) local current, size = 0, self:Count() return function() current = current + 1 local pPanel = self:Element(current - 1).m_pPanel if current <= size and nativeIsValidPanelPointer(pPanel) then return current, pPanel end end end }) panelList = typeof("$[?]", CUtlVector_Constructor_t)(1)[0] panelArrayOffset = cast("unsigned int*", cast("uintptr_t**", UIEngine:getInstance())[0][36] + 21)[0] panelArray = cast(panelList, cast("uintptr_t", UIEngine:getInstance()) + panelArrayOffset) panorama.hasPanel = function(panelName) for i, v in ipairs(panelArray) do local curPanelName = ffi.string(nativeGetID(v)) if curPanelName == panelName then return true end end return false end panorama.getPanel = function(panelName, fallback) local cachedPanel = panorama.panelIDs[panelName] if cachedPanel ~= nil and nativeIsValidPanelPointer(cachedPanel) and ffi.string(nativeGetID(cachedPanel)) == panelName then return cachedPanel end panorama.panelIDs = { } local pPanel = nullptr for i, v in ipairs(panelArray) do local curPanelName = ffi.string(nativeGetID(v)) if curPanelName ~= "" then panorama.panelIDs[curPanelName] = v if curPanelName == panelName then pPanel = v break end end end if pPanel == nullptr then if fallback ~= nil then pPanel = panorama.getPanel(fallback) else error(("Failed to get target panel %s (EAX == 0)"):format(tostring(panelName))) end end return pPanel end panorama.runScript = function(jsCode, panel, pathToXMLContext) if panel == nil then panel = panorama.getPanel("CSGOJsRegistration") end if pathToXMLContext == nil then pathToXMLContext = "panorama/layout/base.xml" end if not nativeIsValidPanelPointer(panel) then error("Invalid panel pointer (EAX == 0)") end return nativeCompileRunScript(panel, jsCode, pathToXMLContext, 8, 10, false) end panorama.loadstring = function(jsCode, panel) if panel == nil then panel = "CSGOJsRegistration" end local fallback = "CSGOJsRegistration" if panel == "CSGOMainMenu" then fallback = "CSGOHub" end if panel == "CSGOHub" then fallback = "CSGOMainMenu" end return Script:loadstring(("(()=>{%s})"):format(jsCode), panorama.getPanel(panel, fallback)) end panorama.open = function(panel) if panel == nil then panel = "CSGOJsRegistration" end local fallback = "CSGOJsRegistration" if panel == "CSGOMainMenu" then fallback = "CSGOHub" end if panel == "CSGOHub" then fallback = "CSGOMainMenu" end return HandleScope()(function() return Context(Isolate():getCurrentContext()):global():toValueChecked():toLua(), panorama.GetPanel(panel, fallback) end) end panorama.GetPanel = panorama.getPanel panorama.RunScript = panorama.runScript panorama.panelArray = panelArray panorama.info = _INFO setmetatable(panorama, { __tostring = function(self) return ("luv8 panorama library v%.1f"):format(_INFO._VERSION) end, __index = function(self, key) if panorama.hasPanel(key) then return panorama.open(key) end return panorama.open()[key] end }) return panorama 
end)()

local ui = (function() 
    local ui = { } ui.__type = { group = -1, button = 0, keybind = 1, text_input = 2, text = 3, separator = 4, list = 5, checkbox = 6, color_picker = 7, multi_selection = 8, selection = 9, slider = 10 } ui.__metasave = true ui.__data = { } ui.create = function(_group, _column) local data = { group = _group, column = _column, id = ui.__type.group } menu.set_group_column(_group, _column) ui.__index = ui return setmetatable(data, ui) end function ui:create_element(_id, _name, _options) local ref = nil if _id == ui.__type.button then ref = menu.add_button( self.group, _name, _options.fn ) elseif _id == ui.__type.checkbox then ref = menu.add_checkbox( self.group, _name, _options.default_value ) elseif _id == ui.__type.color_picker then ref = _options.parent.ref:add_color_picker( _name, _options.default_value, _options.alpha ) elseif _id == ui.__type.keybind then ref = _options.parent.ref:add_keybind( _name, _options.default_value ) elseif _id == ui.__type.list then ref = menu.add_list( self.group, _name, _options.items, _options.visible_count ) elseif _id == ui.__type.multi_selection then ref = menu.add_multi_selection( self.group, _name, _options.items, _options.visible_count ) elseif _id == ui.__type.selection then ref = menu.add_selection( self.group, _name, _options.items, _options.visible_count ) elseif _id == ui.__type.slider then ref = menu.add_slider( self.group, _name, _options.min, _options.max, _options.step, _options.precision, _options.suffix ) elseif _id == ui.__type.text_input then ref = menu.add_text_input( self.group, _name ) elseif _id == ui.__type.text then ref = menu.add_text( self.group, _name ) elseif _id == ui.__type.separator then ref = menu.add_separator( self.group ) end local data = { name = _name, id = _id, ref = ref, group = self.group, get = function(self, _item) if self.id == ui.__type.multi_selection then return self.ref:get(_item) else return self.ref:get() end end } if not ui.__data[self.group] then ui.__data[self.group] = { } end ui.__data[self.group][_name] = data if ui.__metasave then if not ui[self.group] then ui[self.group] = { } end ui[self.group][_name] = data self[_name] = data end return setmetatable(data, ui) end function ui:button(_name, _fn) _fn = _fn or function() end return self:create_element(ui.__type.button, _name, { fn = _fn }) end function ui:checkbox(_name, _default_value) return self:create_element(ui.__type.checkbox, _name, { default_value = _default_value }) end function ui:color_picker(_parent, _name, _default_value, _alpha) return self:create_element(ui.__type.color_picker, _name, { parent = _parent, default_value = _default_value, alpha = _alpha }) end function ui:keybind(_parent, _name, _default_value) return self:create_element(ui.__type.keybind, _name, { parent = _parent, default_value = _default_value }) end function ui:list(_name, _items, _visible_count) return self:create_element(ui.__type.list, _name, { items = _items, visible_count = _visible_count }) end function ui:multi_selection(_name, _items, _visible_count) return self:create_element(ui.__type.multi_selection, _name, { items = _items, visible_count = _visible_count }) end function ui:selection(_name, _items, _visible_count) return self:create_element(ui.__type.selection, _name, { items = _items, visible_count = _visible_count }) end function ui:slider(_name, _min, _max, _step, _precision, _suffix) return self:create_element(ui.__type.slider, _name, { min = _min, max = _max, step = _step, precision = _precision, suffix = _suffix }) end function ui:text_input(_name) return self:create_element(ui.__type.text_input, _name) end function ui:text(_name, _options) return self:create_element(ui.__type.text, _name, _options) end function ui:separator() return self:create_element(ui.__type.separator, "separator") end ui.export = function() local d = { } for i, v in pairs(ui.__data) do d[i] = { } for i0, v0 in pairs(v) do if v0.id < ui.__type.checkbox then goto skip end if v0.id == ui.__type.multi_selection then local s = { } for i1, v1 in pairs(v0.ref:get_items()) do table.insert(s, {v1, v0.ref:get(v1)}) end table.insert(d[i], {v0.name, s}) elseif v0.id == ui.__type.color_picker then local clr = v0.ref:get() table.insert(d[i], {v0.name, clr.r, clr.g, clr.b, clr.a}) else table.insert(d[i], {v0.name, v0.ref:get()}) end ::skip:: end end return json.encode(d) end ui.import = function(data) local db = json.parse(data) for i, v in pairs(db) do for i0, v0 in pairs(v) do if ui.__data[i] == nil or ui.__data[i][v0[1]] == nil then goto skip end if ui.__data[i][v0[1]].id == ui.__type.multi_selection then for i1, v1 in pairs(v0[2]) do ui.__data[i][v0[1]].ref:set(v1[1], v1[2]) end elseif ui.__data[i][v0[1]].id == ui.__type.color_picker then ui.__data[i][v0[1]].ref:set(color_t(v0[2], v0[3], v0[4], v0[5])) else ui.__data[i][v0[1]].ref:set(v0[2]) end ::skip:: end end end function ui:depend(...) local args = {...} local result = nil for i, v in pairs(args) do local con = nil if type(v[1]) == 'boolean' then con = v[1] else con = v[1].ref:get() == v[2] end if result ~= nil then result = (result and con) else result = con end end if self.id == -1 then menu.set_group_visibility(self.group, result) else self.ref:set_visible(result) end end return ui 
end)()

local etr = (function() 
    local tbl = { } tbl.states = { { name = 'Stand', con = nil }, { name = 'Crouch', con = nil }, { name = 'Slow walk', con = nil }, { name = 'Run', con = nil }, { name = 'Air', con = nil }, { name = 'Air duck', con = nil }, { name = 'Use', con = nil } } function tbl.get_current_state(entity) local sw = menu.find("misc", "main", "movement", "slow walk")[2] local flags, velocity, use_key, jump_key = entity:get_prop("m_fFlags"), math.floor(entity:get_prop("m_vecVelocity"):length()), input.find_key_bound_to_binding("use"), input.find_key_bound_to_binding("jump") local stand_internal = velocity <= 3 local crouch_internal = entity:has_player_flag(e_player_flags.DUCKING) local walk_internal = sw:get() local run_internal = velocity > 3 local air_internal = input.is_key_held(jump_key) or bit.band(flags, bit.lshift(1, 0)) == 0 local use_internal = input.is_key_held(use_key) tbl.states[1].con = stand_internal and not crouch_internal and not air_internal tbl.states[2].con = crouch_internal and not air_internal tbl.states[3].con = walk_internal and not air_internal tbl.states[4].con = run_internal and not air_internal tbl.states[5].con = air_internal and not crouch_internal tbl.states[6].con = air_internal and crouch_internal tbl.states[7].con = use_internal local curr = nil for i, v in pairs(tbl.states) do if v.con and curr == nil then curr = v.name end end return curr end function tbl.get_states() local names = { } for _, v in pairs(tbl.states) do table.insert(names, v.name) end return names end function tbl.set_jitter(j_type, delay) if type(j_type) ~= 'string' then client.log_screen('set_jitter expected string value') return end if type(delay) ~= 'number' then client.log_screen('set_jitter expected number value') return end if j_type == 'delayed' then return math.floor(globals.tick_count() % delay) end end local def = { } def.defensive, def.last_sim_time = 0, 0 function tbl.is_defensive(entity) local ent, tickcount = entity, globals.tick_count() local sim_time = client.time_to_ticks(ent:get_prop("m_flSimulationTime")) local diff = sim_time - def.last_sim_time if diff < 0 then def.defensive = tickcount + math.abs(diff) - math.floor(client.time_to_ticks(engine.get_latency(e_latency_flows))) end def.last_sim_time = sim_time return def.defensive > tickcount end function tbl.round(float) return math.floor(float + .5) end function tbl.clamp(value, min_value, max_value) if value < min_value then return min_value elseif value > max_value then return max_value else return value end end function tbl.lerp(start, end_pos, time) if type(end_pos) == "number" and math.floor(end_pos) ~= end_pos then goto skip end ::skip:: local t, threshold = tbl.clamp(globals.frame_time() * time, 0, 1), 0.3 if t >= 1 - threshold then return end_pos else return start + (end_pos - start) * t end end function tbl.closest_point_on_ray(ray_from, ray_to, desired_point) local to = desired_point - ray_from local direction = ray_to - ray_from local ray_length = #direction direction.x = direction.x / ray_length direction.y = direction.y / ray_length direction.z = direction.z / ray_length local direction_along = direction.x * to.x + direction.y * to.y + direction.z * to.z if direction_along < 0 then return ray_from end if direction_along > ray_length then return ray_to end return vec3_t( ray_from.x + direction.x * direction_along, ray_from.y + direction.y * direction_along, ray_from.z + direction.z * direction_along ) end function tbl.dist_to(pos1, pos2) local dx = pos1.x - pos2.x local dy = pos1.y - pos2.y local dz = pos1.z - pos2.z return math.sqrt(dx * dx + dy * dy + dz * dz) end function tbl.glow(position, size, round, color, glow_size) local x, y = position.x, position.y local w, h = size.x, size.y local r, g, b, a = color.r, color.g, color.b, color.a for radius = 2, math.floor(glow_size) do local radius_glow = radius / 2 local alpha = math.floor(a / glow_size * (glow_size - radius)) render.rect(vec2_t(x - radius_glow, y - radius_glow), vec2_t(w + radius_glow * 2, h + radius_glow * 2), color_t(r, g, b, alpha), round) end end function tbl.color_anim(r, g, b, a) return color_t(math.floor(r + 0.5), math.floor(g + 0.5), math.floor(b + 0.5), a == nil and 255 or (math.floor(a + 0.5) or 255)) end function tbl.animated_text(font, text, color2, x, y, speed) if speed == 0 then speed = 1 else speed = speed end local data, totalWidth = {}, 0 local len, two_pi = #text, math.pi * 1.5 local textOffset = vec2_t(x, y) local clr1 = color_t(0, 0, 0, 155) for idx = 1, len do local modifier = two_pi / len * idx local char = text:sub(idx, idx) local charWidth = render.get_text_size(font, char).x data[idx] = {totalWidth, char, modifier} totalWidth = totalWidth + charWidth end totalWidth = totalWidth * 0.5 return function() local time = -globals.cur_time() * math.pi * speed local headerOffset = textOffset - vec2_t(totalWidth, 0) for _, char in pairs(data) do local charPosition = headerOffset + vec2_t(char[1], 0) local fadeValue = math.sin(time + char[3]) * 0.5 + 0.5 local color = clr1:fade(color2, fadeValue) render.text(font, char[2], charPosition, color) end end end c_render = { } c_render.frame = function(pos_start, pos_end, clr, style, round, alpha_modifier) if style == nil then style = 1 end if round == nil then round = 5 end if alpha_modifier == nil then alpha_modifier = 1 end if style == 1 then render.rect(pos_start, pos_end - pos_start, color_t(255, 255, 255, math.floor(150 * alpha_modifier)), round) end end c_render.drag_frame = function(pos_start, pos_end, clr) if not menu.is_open() then return end c_render.frame(pos_start, pos_end, clr, 1, 2, 1) end local c_anim = { } c_anim.data = { } c_anim.new = function(name, value, time) if (c_anim.data[name] == nil) then c_anim.data[name] = value end c_anim.data[name] = tbl.lerp(c_anim.data[name], value, time) return c_anim.data[name] end c_anim.fn_smooth_lerp = function(start, end_pos, time) if math.abs(start - end_pos) < 1 or start == end_pos then return end_pos end local value = tbl.lerp(start, end_pos, time) return value end local c_window = { } c_window.data = { } c_window.target = "" tbl.new = function(name, pos, size) local wnd = { name = name, pos = pos, d_pos = vec2_t(0, 0), size = size, anim = 0, drag = false, } c_window.__index = c_window table.insert(c_window.data, wnd) return setmetatable(wnd, c_window) end function c_window:lerp(value_name, end_value, time) local start_val = self[value_name] local clamped_time = tbl.clamp(globals.frame_time() * time, 0, 0.5) self[value_name] = vec2_t((end_value.x - start_val.x) * clamped_time + start_val.x, (end_value.y - start_val.y) * clamped_time + start_val.y) end function c_window:limit_pos(screen_size) if self.pos.x <= 0 then self.pos.x = 0 end if self.pos.x + self.size.x >= screen_size.x - 1 then self.pos.x = screen_size.x - self.size.x - 1 end if self.pos.y <= 0 then self.pos.y = 0 end if self.pos.y + self.size.y >= screen_size.y - 1 then self.pos.y = screen_size.y - self.size.y - 1 end end function c_window:is_in_area(mouse_pos) return mouse_pos.x >= self.pos.x - 10 and mouse_pos.x <= self.pos.x + self.size.x + 10 and mouse_pos.y >= self.pos.y - 10 and mouse_pos.y <= self.pos.y + self.size.y + 10 end function c_window:update() local screen = render.get_screen_size() local mouse_pos = input.get_mouse_pos() local left_pressed = input.is_key_held(e_keys.MOUSE_LEFT) local right_pressed = input.is_key_pressed(e_keys.MOUSE_RIGHT) local can_drag = self:is_in_area(mouse_pos) local drag_active = self.drag local target = c_window.target local is_target = (target == "" or target == self.name) local clr = (target == self.name or can_drag and menu.is_open()) and color_t(255, 255, 255, 255) or color_t(100, 100, 100, 100) c_render.drag_frame(self.pos - vec2_t(10, 10), self.pos + self.size + vec2_t(10, 10), clr) local alpha_var = 0 if can_drag and menu.is_open() and is_target then alpha_var = c_anim.new("Drag alpha" .. self.name, 255, 4) render.text(render.get_default_font(), "Press M2 to center", self.pos + vec2_t(-5, self.size.y + 10), color_t(255, 255, 255, 255)) else alpha_var = c_anim.new("Drag alpha" .. self.name, 0, 4) end if menu.is_open() and can_drag and left_pressed and not drag_active and is_target then self.drag = true self.d_pos = self.pos - mouse_pos elseif menu.is_open() and can_drag and right_pressed and not drag_active and is_target then self.pos.x = screen.x / 2 - self.size.x / 2 end if not left_pressed then c_window.target = "" self.drag = false end if drag_active and menu.is_open() then c_window.target = self.name new = mouse_pos + self.d_pos self.pos = new self:limit_pos(screen) end end local charbuffer = ffi.typeof("unsigned char[?]") local uintbuffer = ffi.typeof("unsigned int[?]") local DEFLATE_MAX_BLOCK_SIZE = 65535 local function putBigUint32(val, tbl, index) for i = 0, 3 do tbl[index + i] = bit.band(bit.rshift(val, (3 - i) * 8), 0xFF) end end local Png = {} Png.__index = Png function Png:writeBytes(data, index, len) index = index or 1 len = len or #data for i=index,index+len-1 do table.insert(self.output, string.char(data[i])) end end function Png:write(pixels) local count = #pixels local pixelPointer = 1 while count > 0 do if self.positionY >= self.height then error("Pixels already written") end if self.deflateFilled == 0 then local size = DEFLATE_MAX_BLOCK_SIZE; if (self.uncompRemain < size) then size = self.uncompRemain end local header = { bit.band((self.uncompRemain <= DEFLATE_MAX_BLOCK_SIZE and 1 or 0), 0xFF), bit.band(bit.rshift(size, 0), 0xFF), bit.band(bit.rshift(size, 8), 0xFF), bit.band(bit.bxor(bit.rshift(size, 0), 0xFF), 0xFF), bit.band(bit.bxor(bit.rshift(size, 8), 0xFF), 0xFF), } self:writeBytes(header) self:crc32(header, 1, #header) end assert(self.positionX < self.lineSize and self.deflateFilled < DEFLATE_MAX_BLOCK_SIZE); if (self.positionX == 0) then local b = {0} self:writeBytes(b) self:crc32(b, 1, 1) self:adler32(b, 1, 1) self.positionX = self.positionX + 1 self.uncompRemain = self.uncompRemain - 1 self.deflateFilled = self.deflateFilled + 1 else local n = DEFLATE_MAX_BLOCK_SIZE - self.deflateFilled; if (self.lineSize - self.positionX < n) then n = self.lineSize - self.positionX end if (count < n) then n = count; end assert(n > 0); self:writeBytes(pixels, pixelPointer, n) self:crc32(pixels, pixelPointer, n); self:adler32(pixels, pixelPointer, n); count = count - n; pixelPointer = pixelPointer + n; self.positionX = self.positionX + n; self.uncompRemain = self.uncompRemain - n; self.deflateFilled = self.deflateFilled + n; end if (self.deflateFilled >= DEFLATE_MAX_BLOCK_SIZE) then self.deflateFilled = 0; end if (self.positionX == self.lineSize) then self.positionX = 0; self.positionY = self.positionY + 1; if (self.positionY == self.height) then local footer = { 0, 0, 0, 0, 0, 0, 0, 0, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4E, 0x44, 0xAE, 0x42, 0x60, 0x82, } putBigUint32(self.adler, footer, 1) self:crc32(footer, 1, 4) putBigUint32(self.crc, footer, 5) self:writeBytes(footer) self.done = true end end end end function Png:crc32(data, index, len) self.crc = bit.bnot(self.crc) for i = index, index + len - 1 do local byte = data[i] for j = 0, 7 do local nbit = bit.band(bit.bxor(self.crc, bit.rshift(byte, j)), 1); self.crc = bit.bxor(bit.rshift(self.crc, 1), bit.band((-nbit), 0xEDB88320)); end end self.crc = bit.bnot(self.crc) end function Png:adler32(data, index, len) local s1 = bit.band(self.adler, 0xFFFF) local s2 = bit.rshift(self.adler, 16) for i = index, index + len - 1 do s1 = (s1 + data[i]) % 65521 s2 = (s2 + s1) % 65521 end self.adler = bit.bor(bit.lshift(s2, 16), s1) end local function begin(width, height, colorMode) colorMode = colorMode or "rgb" local bytesPerPixel, colorType if colorMode == "rgb" then bytesPerPixel, colorType = 3, 2 elseif colorMode == "rgba" then bytesPerPixel, colorType = 4, 6 else error("Invalid color mode") end local state = setmetatable({ width = width, height = height, done = false, output = {} }, Png) state.lineSize = width * bytesPerPixel + 1 state.uncompRemain = state.lineSize * height local numBlocks = math.ceil(state.uncompRemain / DEFLATE_MAX_BLOCK_SIZE) local idatSize = numBlocks * 5 + 6 idatSize = idatSize + state.uncompRemain; local header = { 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44, 0x52, 0, 0, 0, 0, 0, 0, 0, 0, 0x08, colorType, 0x00, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0, 0x49, 0x44, 0x41, 0x54, 0x08, 0x1D, } putBigUint32(width, header, 17) putBigUint32(height, header, 21) putBigUint32(idatSize, header, 34) state.crc = 0 state:crc32(header, 13, 17) putBigUint32(state.crc, header, 30) state:writeBytes(header) state.crc = 0 state:crc32(header, 38, 6); state.adler = 1 state.positionX = 0 state.positionY = 0 state.deflateFilled = 0 return state end ffi.cdef([[ typedef struct { void* steam_client; void* steam_user; void* steam_friends; void* steam_utils; void* steam_matchmaking; void* steam_user_stats; void* steam_apps; void* steam_matchmakingservers; void* steam_networking; void* steam_remotestorage; void* steam_screenshots; void* steam_http; void* steam_unidentifiedmessages; void* steam_controller; void* steam_ugc; void* steam_applist; void* steam_music; void* steam_musicremote; void* steam_htmlsurface; void* steam_inventory; void* steam_video; } S_steamApiCtx_t; ]]) local pS_SteamApiCtx = ffi.cast( "S_steamApiCtx_t**", ffi.cast( "char*", memory.find_pattern( "client.dll", "FF 15 ?? ?? ?? ?? B9 ?? ?? ?? ?? E8 ?? ?? ?? ?? 6A" ) ) + 7 )[0] or error("invalid interface", 2) local native_ISteamFriends = ffi.cast("void***", pS_SteamApiCtx.steam_friends) local native_ISteamUtils = ffi.cast("void***", pS_SteamApiCtx.steam_utils) local native_ISteamFriends_GetSmallFriendAvatar = ffi.cast("int(__thiscall*)(void*, uint64_t)", native_ISteamFriends[0][34]) local native_ISteamFriends_GetMediumFriendAvatar = ffi.cast("int(__thiscall*)(void*, uint64_t)", native_ISteamFriends[0][35]) local native_ISteamFriends_GetLargeFriendAvatar = ffi.cast("int(__thiscall*)(void*, uint64_t)", native_ISteamFriends[0][36]) local native_ISteamUtils_GetImageSize = ffi.cast("bool(__thiscall*)(void*, int, uint32_t*, uint32_t*)", native_ISteamUtils[0][5]) local native_ISteamUtils_GetImageRGBA = ffi.cast("bool(__thiscall*)(void*, int, unsigned char*, int)", native_ISteamUtils[0][6]) local steam_avatars = {} get_steam_avatar = function(steamid, size) local cache_key = string.format("%s_%s", steamid, size) local huy local image_bytes = "" if steam_avatars[cache_key] == nil then local func if size == nil then func = native_ISteamFriends_GetSmallFriendAvatar elseif size > 64 then func = native_ISteamFriends_GetLargeFriendAvatar elseif size > 32 then func = native_ISteamFriends_GetMediumFriendAvatar else func = native_ISteamFriends_GetSmallFriendAvatar end local handle = func(native_ISteamFriends , tonumber(steamid:sub(4, -1)) + 76500000000000000ULL) if handle > 0 then local width = uintbuffer(1) local height = uintbuffer(1) if native_ISteamUtils_GetImageSize(native_ISteamUtils, handle, width, height) then if width[0] > 0 and height[0] > 0 then local rgba_buffer_size = width[0] * height[0] * 4 local rgba_buffer = charbuffer(rgba_buffer_size) if native_ISteamUtils_GetImageRGBA(native_ISteamUtils, handle, rgba_buffer, rgba_buffer_size) then local png = begin(width[0], height[0], "rgba") for x = 0 , width[0] - 1 do for y = 0, height[0] - 1 do local pizda = x * (height[0] * 4) + y * 4 png:write {rgba_buffer[pizda] , rgba_buffer[pizda + 1] , rgba_buffer[pizda + 2] , rgba_buffer[pizda + 3]} end end for i = 1, #png.output do image_bytes = image_bytes .. png.output[i] end steam_avatars[cache_key] = render.load_image_buffer(image_bytes) end end end elseif handle ~= -1 then steam_avatars[cache_key] = nil end end if steam_avatars[cache_key] then return steam_avatars[cache_key] end end tbl.data = {} tbl.default_image = render.load_image_buffer("\xFF\xD8\xFF\xE0\x00\x10\x4A\x46\x49\x46\x00\x01\x01\x00\x00\x01\x00\x01\x00\x00\xFF\xFE\x00\x3B\x43\x52\x45\x41\x54\x4F\x52\x3A\x20\x67\x64\x2D\x6A\x70\x65\x67\x20\x76\x31\x2E\x30\x20\x28\x75\x73\x69\x6E\x67\x20\x49\x4A\x47\x20\x4A\x50\x45\x47\x20\x76\x36\x32\x29\x2C\x20\x71\x75\x61\x6C\x69\x74\x79\x20\x3D\x20\x38\x30\x0A\xFF\xDB\x00\x43\x00\x06\x04\x05\x06\x05\x04\x06\x06\x05\x06\x07\x07\x06\x08\x0A\x10\x0A\x0A\x09\x09\x0A\x14\x0E\x0F\x0C\x10\x17\x14\x18\x18\x17\x14\x16\x16\x1A\x1D\x25\x1F\x1A\x1B\x23\x1C\x16\x16\x20\x2C\x20\x23\x26\x27\x29\x2A\x29\x19\x1F\x2D\x30\x2D\x28\x30\x25\x28\x29\x28\xFF\xDB\x00\x43\x01\x07\x07\x07\x0A\x08\x0A\x13\x0A\x0A\x13\x28\x1A\x16\x1A\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\x28\xFF\xC0\x00\x11\x08\x00\x40\x00\x40\x03\x01\x22\x00\x02\x11\x01\x03\x11\x01\xFF\xC4\x00\x1F\x00\x00\x01\x05\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\xFF\xC4\x00\xB5\x10\x00\x02\x01\x03\x03\x02\x04\x03\x05\x05\x04\x04\x00\x00\x01\x7D\x01\x02\x03\x00\x04\x11\x05\x12\x21\x31\x41\x06\x13\x51\x61\x07\x22\x71\x14\x32\x81\x91\xA1\x08\x23\x42\xB1\xC1\x15\x52\xD1\xF0\x24\x33\x62\x72\x82\x09\x0A\x16\x17\x18\x19\x1A\x25\x26\x27\x28\x29\x2A\x34\x35\x36\x37\x38\x39\x3A\x43\x44\x45\x46\x47\x48\x49\x4A\x53\x54\x55\x56\x57\x58\x59\x5A\x63\x64\x65\x66\x67\x68\x69\x6A\x73\x74\x75\x76\x77\x78\x79\x7A\x83\x84\x85\x86\x87\x88\x89\x8A\x92\x93\x94\x95\x96\x97\x98\x99\x9A\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xAA\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xCA\xD2\xD3\xD4\xD5\xD6\xD7\xD8\xD9\xDA\xE1\xE2\xE3\xE4\xE5\xE6\xE7\xE8\xE9\xEA\xF1\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\xFA\xFF\xC4\x00\x1F\x01\x00\x03\x01\x01\x01\x01\x01\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\xFF\xC4\x00\xB5\x11\x00\x02\x01\x02\x04\x04\x03\x04\x07\x05\x04\x04\x00\x01\x02\x77\x00\x01\x02\x03\x11\x04\x05\x21\x31\x06\x12\x41\x51\x07\x61\x71\x13\x22\x32\x81\x08\x14\x42\x91\xA1\xB1\xC1\x09\x23\x33\x52\xF0\x15\x62\x72\xD1\x0A\x16\x24\x34\xE1\x25\xF1\x17\x18\x19\x1A\x26\x27\x28\x29\x2A\x35\x36\x37\x38\x39\x3A\x43\x44\x45\x46\x47\x48\x49\x4A\x53\x54\x55\x56\x57\x58\x59\x5A\x63\x64\x65\x66\x67\x68\x69\x6A\x73\x74\x75\x76\x77\x78\x79\x7A\x82\x83\x84\x85\x86\x87\x88\x89\x8A\x92\x93\x94\x95\x96\x97\x98\x99\x9A\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xAA\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xCA\xD2\xD3\xD4\xD5\xD6\xD7\xD8\xD9\xDA\xE2\xE3\xE4\xE5\xE6\xE7\xE8\xE9\xEA\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\xFA\xFF\xDA\x00\x0C\x03\x01\x00\x02\x11\x03\x11\x00\x3F\x00\xF0\x89\xE6\x93\xCF\x93\xF7\x8F\xF7\x8F\xF1\x1F\x5A\x67\x9D\x27\xFC\xF4\x7F\xFB\xE8\xD1\x3F\xFA\xF9\x3F\xDE\x3F\xCE\x99\x40\x0F\xF3\xA4\xFF\x00\x9E\x8F\xFF\x00\x7D\x1A\x3C\xE9\x3F\xE7\xA3\xFF\x00\xDF\x46\x99\x5A\x3A\x2E\x87\xAA\xEB\x93\x34\x5A\x3E\x9D\x75\x7A\xEB\xF7\x84\x11\x17\xDB\xF5\x23\xA7\xE3\x40\x14\x7C\xE9\x3F\xE7\xA3\xFF\x00\xDF\x46\x8F\x3A\x4F\xF9\xE8\xFF\x00\xF7\xD1\xAD\x1D\x6F\xC3\xBA\xCE\x86\x57\xFB\x63\x4B\xBC\xB2\x0F\xC2\xB4\xD1\x15\x56\xFA\x1E\x86\xB2\xE8\x01\xFE\x74\x9F\xF3\xD1\xFF\x00\xEF\xA3\x4F\x82\x69\x3C\xF8\xFF\x00\x78\xFF\x00\x78\x7F\x11\xF5\xA8\x69\xF0\x7F\xAF\x8F\xFD\xE1\xFC\xE8\x00\x9F\xFD\x7C\x9F\xEF\x1F\xE7\x4C\xA7\xCF\xFE\xBE\x4F\xF7\x8F\xF3\xA6\x50\x05\xDD\x13\x4F\x7D\x5B\x59\xB0\xD3\xA2\x60\xB2\x5D\xCF\x1C\x0A\x4F\x62\xCC\x14\x1F\xD6\xBE\xA8\xF1\x77\x88\x74\x8F\x84\x5E\x12\xD3\xED\x34\xDD\x3C\x4A\xD2\x13\x1C\x10\x06\xD9\xBC\x80\x37\x48\xED\x8E\xBC\x8C\xFA\x93\x5F\x27\x5B\xCF\x2D\xB5\xC4\x73\xDB\xC8\xF1\x4D\x13\x07\x49\x11\x8A\xB2\xB0\x39\x04\x11\xD0\x83\x56\xF5\x4D\x67\x53\xD5\xFC\xBF\xED\x5D\x46\xF2\xF7\xCB\xCE\xCF\xB4\x4C\xD2\x6D\xCF\x5C\x64\x9C\x74\x14\x01\xF5\x17\xC3\xEF\x1C\xE9\x9F\x14\x74\xDD\x47\x4B\xD5\x74\xC4\x8A\x54\x4C\xCD\x6C\xED\xE6\x24\x88\x4E\x37\x29\xC0\x20\x83\xF9\x71\x83\x5F\x34\xF8\xDB\x44\x1E\x1C\xF1\x66\xA9\xA4\xAB\x17\x4B\x59\xCA\x23\x1E\xA5\x0F\x2B\x9F\x7C\x11\x5F\x42\x7C\x16\xF0\xA4\x5E\x06\xF0\x9D\xDF\x88\xBC\x40\xE2\xDA\xE6\xE6\x11\x2C\x9E\x67\x1E\x44\x23\x90\x0F\xFB\x47\xA9\x1F\x41\xD6\xBE\x7A\xF1\xAE\xB7\xFF\x00\x09\x1F\x8A\xF5\x4D\x58\x21\x44\xBA\x98\xBA\x29\xEA\x13\xA2\x83\xEF\x80\x28\x03\x16\x9F\x07\xFA\xF8\xFF\x00\xDE\x1F\xCE\x99\x4F\x83\xFD\x7C\x7F\xEF\x0F\xE7\x40\x04\xFF\x00\xEB\xE4\xFF\x00\x78\xFF\x00\x3A\x65\x3E\x7F\xF5\xF2\x7F\xBC\x7F\x9D\x32\x80\x0A\xED\xFE\x0B\x69\x96\xDA\xBF\xC4\xAD\x1E\xDA\xF6\x31\x24\x0A\xCF\x31\x46\x19\x0C\x51\x19\x86\x7D\xB2\x05\x71\x15\xD3\xFC\x36\xD3\x75\x4D\x5F\xC6\x16\x76\x5A\x0E\xA0\x74\xED\x42\x45\x90\xC7\x72\x19\x97\x68\x08\x49\xE5\x79\xE4\x02\x3F\x1A\x00\xF5\x1F\xDA\x6F\xC4\xF7\x62\xFE\xCF\xC3\x70\x31\x8E\xCF\xCA\x5B\xA9\xF1\xD6\x46\x2C\xC1\x41\xF6\x1B\x73\xF5\x3E\xD5\xE0\xF5\xD8\xFC\x56\xD1\xF5\xAD\x13\xC4\xE9\x6B\xE2\x3D\x50\xEA\x97\xA6\xDD\x1C\x4E\x5D\x9B\x08\x4B\x61\x72\xDC\xF5\x07\xF3\xAE\x3A\x80\x0A\x7C\x1F\xEB\xE3\xFF\x00\x78\x7F\x3A\x65\x3E\x0F\xF5\xF1\xFF\x00\xBC\x3F\x9D\x00\x13\xFF\x00\xAF\x93\xFD\xE3\xFC\xE9\x95\x34\xF0\xC9\xE7\xC9\xFB\xB7\xFB\xC7\xF8\x4F\xAD\x33\xC9\x93\xFE\x79\xBF\xFD\xF2\x68\x01\x95\x7B\x43\xD5\xEF\xF4\x2D\x4A\x2D\x43\x49\xB8\x6B\x6B\xC8\xC1\x09\x22\x80\x48\xC8\x20\xF5\x04\x74\x26\xAA\x79\x32\x7F\xCF\x37\xFF\x00\xBE\x4D\x1E\x4C\x9F\xF3\xCD\xFF\x00\xEF\x93\x40\x1A\x1E\x21\xD7\xB5\x3F\x11\x5F\x8B\xDD\x6A\xED\xAE\xEE\x82\x08\xC4\x8C\xA0\x1D\xA0\x92\x07\x00\x7A\x9A\xCC\xA7\xF9\x32\x7F\xCF\x37\xFF\x00\xBE\x4D\x1E\x4C\x9F\xF3\xCD\xFF\x00\xEF\x93\x40\x0C\xA7\xC1\xFE\xBE\x3F\xF7\x87\xF3\xA3\xC9\x93\xFE\x79\xBF\xFD\xF2\x69\xF0\x43\x27\x9F\x1F\xEE\xDF\xEF\x0F\xE1\x3E\xB4\x01\xFF\xD9") tbl.fn_create_item = function(name) tbl.data[name] = {} tbl.data[name].url = nil tbl.data[name].image = nil tbl.data[name].loaded = false tbl.data[name].loading = false end tbl.get_avatar = function(name, entindex) if tbl.data[name] and tbl.data[name].loaded then return tbl.data[name].image end if tbl.data[name] == nil then tbl.fn_create_item(name) local steamID3, steam_id = entity_list.get_entity(entindex):get_steamids() if #steam_id<5 then return end if steam_id == nil or tbl.default_image == nil then return nil end tbl.data[name].image = get_steam_avatar(steam_id, 64) tbl.data[name].loaded = true end return tbl.default_image end local specs = { } local spectating_players = function(player) local buffer = { } local frames = 8 * global_vars.frame_time() local players = entity_list.get_entities_by_name('CCSPlayer') for tbl_idx, player_pointer in pairs(players) do if player_pointer:get_index() ~= player:get_index() then if player_pointer == nil then return end if not player_pointer:get_prop("m_iHealth") ~= 0 and not player_pointer:is_dormant() then local spectatingMode = player_pointer:get_prop('m_iObserverMode') local spectatingPlayer = player_pointer:get_prop('m_hObserverTarget') if spectatingPlayer then if spectatingMode >= 4 or spectatingMode <= 5 then local spectatingEntity = entity_list.get_entity(spectatingPlayer) if spectatingEntity ~= nil and spectatingEntity:get_index() == player:get_index() then if specs[player_pointer:get_index()] == nil then specs[player_pointer:get_index()] = { alpha = 0, offset = 0, active = true } end specs[player_pointer:get_index()].active = true specs[player_pointer:get_index()].alpha = specs[player_pointer:get_index()].alpha + frames if specs[player_pointer:get_index()].alpha > 1 then specs[player_pointer:get_index()].alpha = 1 end table.insert(buffer, 1, { ['alpha'] = specs[player_pointer:get_index()].alpha, ['id'] = player_pointer:get_index(), ['name'] = player_pointer:get_name(), }) elseif specs[player_pointer:get_index()] ~= nil then specs[player_pointer:get_index()].active = false specs[player_pointer:get_index()].alpha = specs[player_pointer:get_index()].alpha - frames if specs[player_pointer:get_index()].alpha <= 0 then specs[player_pointer:get_index()] = nil end end end else specs[player_pointer:get_index()] = nil end else specs[player_pointer:get_index()] = nil end end end return buffer end callbacks.add(e_callbacks.EVENT, function () if buffer == nil then return end for index, value in pairs(buffer) do if spectating_players(value) == nil then print() end end end, 'round_start') tbl.get_spectators = function() if not engine.is_connected() or entity_list.get_local_player() == nil then return end local local_player = entity_list.get_local_player() if local_player:get_prop("m_iHealth") > 0 then return spectating_players(local_player) else local m_hObserverTarget = local_player:get_prop('m_hObserverTarget') if m_hObserverTarget then local targetEntity = entity_list.get_entity(m_hObserverTarget) if targetEntity ~= nil then if targetEntity:is_player() and targetEntity:is_alive() then return spectating_players(targetEntity) end end end end end function tbl.active_weapon() if ragebot.get_active_cfg() == 0 then return "auto" elseif ragebot.get_active_cfg() == 1 then return "scout" elseif ragebot.get_active_cfg() == 2 then return "awp" elseif ragebot.get_active_cfg() == 3 then return "deagle" elseif ragebot.get_active_cfg() == 4 then return "revolver" elseif ragebot.get_active_cfg() == 5 then return "pistols" else return "other" end end function tbl.get_fake(lp) return math.abs(lp:get_prop("m_flPoseParameter", 11) * 120 - 60) end ffi.cdef [[ typedef int(__thiscall* get_clipboard_text_count)(void*); typedef void(__thiscall* set_clipboard_text)(void*, const char*, int); typedef void(__thiscall* get_clipboard_text)(void*, int, const char*, int); ]] local VGUI_System010 = memory.create_interface("vgui2.dll", "VGUI_System010") or error("Error finding VGUI_System010") local VGUI_System = ffi.cast(ffi.typeof('void***'), VGUI_System010) local get_clipboard_text_count = ffi.cast("get_clipboard_text_count", VGUI_System[0][7]) or error("get_clipboard_text_count Invalid") local set_clipboard_text = ffi.cast("set_clipboard_text", VGUI_System[0][9]) or error("set_clipboard_text Invalid") local get_clipboard_text = ffi.cast("get_clipboard_text", VGUI_System[0][11]) or error("get_clipboard_text Invalid") tbl.clipboard_import = function() local text_length = get_clipboard_text_count(VGUI_System) if text_length <= 0 then return "" end local buffer = ffi.new("char[?]", text_length) local buffer_size = text_length * ffi.sizeof("char[?]", text_length) get_clipboard_text(VGUI_System, 0, buffer, buffer_size) return ffi.string(buffer, text_length - 1) end tbl.clipboard_export = function(string) if not string then return end set_clipboard_text(VGUI_System, string, string:len()) end tbl.__index = { } tbl.class = ffi.cast(ffi.typeof("void***"), memory.create_interface("filesystem_stdio.dll", "VBaseFileSystem011")) tbl.v_table = tbl.class[0] tbl.full_class = ffi.cast("void***", memory.create_interface("filesystem_stdio.dll", "VFileSystem017")) tbl.v_fltable = tbl.full_class[0] tbl.casts = { read_file = ffi.cast("int (__thiscall*)(void*, void*, int, void*)", tbl.v_table[0]), write_file = ffi.cast("int (__thiscall*)(void*, void const*, int, void*)", tbl.v_table[1]), open_file = ffi.cast("void* (__thiscall*)(void*, const char*, const char*, const char*)", tbl.v_table[2]), close_file = ffi.cast("void (__thiscall*)(void*, void*)", tbl.v_table[3]), file_size = ffi.cast("unsigned int (__thiscall*)(void*, void*)", tbl.v_table[7]), file_exists = ffi.cast("bool (__thiscall*)(void*, const char*, const char*)", tbl.v_table[10]), delete_file = ffi.cast("void (__thiscall*)(void*, const char*, const char*)", tbl.v_fltable[20]), rename_file = ffi.cast("bool (__thiscall*)(void*, const char*, const char*, const char*)", tbl.v_fltable[21]), create_dir = ffi.cast("void (__thiscall*)(void*, const char*, const char*)", tbl.v_fltable[22]), is_dir = ffi.cast("bool (__thiscall*)(void*, const char*, const char*)", tbl.v_fltable[23]), } local filesystem = memory.create_interface("filesystem_stdio.dll", "VFileSystem017") local call = ffi.cast(ffi.typeof("void***"), filesystem) ffi.cdef([[ typedef void (__thiscall* AddSearchPath)(void*, const char*, const char*); typedef void (__thiscall* RemoveSearchPaths)(void*, const char*); typedef const char* (__thiscall* FindNext)(void*, int); typedef bool (__thiscall* FindIsDirectory)(void*, int); typedef void (__thiscall* FindClose)(void*, int); typedef const char* (__thiscall* FindFirstEx)(void*, const char*, const char*, int*); typedef long (__thiscall* GetFileTime)(void*, const char*, const char*); ]]) local add_search_path = ffi.cast("AddSearchPath", call[0][11]) local remove_search_paths = ffi.cast("RemoveSearchPaths", call[0][14]) local find_next = ffi.cast("FindNext", call[0][33]) local find_is_dir = ffi.cast("FindIsDirectory", call[0][34]) local find_close = ffi.cast("FindClose", call[0][35]) local find_first_ex = ffi.cast("FindFirstEx", call[0][36]) tbl.modes = { ["r"] = "r", ["w"] = "w", ["a"] = "a", ["r+"] = "r+", ["w+"] = "w+", ["a+"] = "a+", ["rb"] = "rb", ["wb"] = "wb", ["ab"] = "ab", ["rb+"] = "rb+", ["wb+"] = "wb+", ["ab+"] = "ab+", } tbl.open = function(file, mode, id) if (not tbl.modes[mode]) then error("File mode error!") end local self = setmetatable({ file = file, mode = mode, path_id = id, handle = tbl.casts.open_file(tbl.class, file, mode, id) }, tbl) return self end tbl.close = function(fs) tbl.casts.close_file(tbl.class, fs.handle) end tbl.exists = function(file, id) return tbl.casts.file_exists(tbl.class, file, id) end tbl.get_size = function(fs) return tbl.casts.file_size(tbl.class, fs.handle) end tbl.write = function(path, buffer) local fs = tbl.open(path, "w", "MOD") tbl.casts.write_file(tbl.class, buffer, #buffer, fs.handle) tbl.close(fs) end tbl.append = function(path, buffer) local fs = tbl.open(path, "a", "MOD") tbl.casts.write_file(tbl.class, buffer, #buffer, fs.handle) tbl.close(fs) end tbl.read = function(path) local fs = tbl.open(path, "r", "MOD") local size = tbl.get_size(fs) local output = ffi.new("char[?]", size + 1) tbl.casts.read_file(tbl.class, output, size, fs.handle) tbl.close(fs) return ffi.string(output) end tbl.rename = function(old_path, new_path, id) return tbl.casts.rename_file(tbl.full_class, old_path, new_path, id) end tbl.delete = function(file, id) tbl.casts.delete_file(tbl.full_class, file, id) end tbl.create_directory = function(path, id) tbl.casts.create_dir(tbl.full_class, path, id) end tbl.is_directory = function(path, id) return tbl.casts.is_dir(tbl.full_class, path, id) end tbl.find_next = function(handle) local file = tbl.casts.find_next(tbl.full_class, handle) if (file == 0) then return nil end return ffi.string(file) end tbl.find_is_directory = function(handle) return tbl.casts.find_is_dir(tbl.full_class, handle) end tbl.get_files = function() local file_handle = ffi.new("int[1]") remove_search_paths(call, "etrium_configs") add_search_path(call, "./csgo/etrium/", "etrium_configs") local file_names = { } local file = find_first_ex(call, "*", "etrium_configs", file_handle) while file ~= nil do local file_name = ffi.string(file) if find_is_dir(call, file_handle[0]) == false and file_name:find("[.]etr") then table.insert(file_names, file_name) end file = find_next(call, file_handle[0]) end find_close(call, file_handle[0]) return file_names end return tbl
end)()

local base64 = (function() 
    local base64={}local b,c,d=bit.lshift,bit.rshift,bit.band;local e,f,g,h,i,j,tostring,error,pairs=string.char,string.byte,string.gsub,string.sub,string.format,table.concat,tostring,error,pairs;local k=function(l,m,n)return d(c(l,m),b(1,n)-1)end;local function o(p)local q,r={},{}for s=1,65 do local t=f(h(p,s,s))or 32;if r[t]~=nil then error("invalid alphabet: duplicate character "..tostring(t),3)end;q[s-1]=t;r[t]=s-1 end;return q,r end;local u,v={},{}u["base64"],v["base64"]=o("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")u["base64url"],v["base64url"]=o("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")local w={__index=function(x,y)if type(y)=="string"and y:len()==64 or y:len()==65 then u[y],v[y]=o(y)return x[y]end end}setmetatable(u,w)setmetatable(v,w)function base64.encode(z,q)q=u[q or"base64"]or error("invalid alphabet specified",2)z=tostring(z)local A,B,C={},1,#z;local D=C%3;local E={}for s=1,C-D,3 do local F,G,H=f(z,s,s+2)local l=F*0x10000+G*0x100+H;local I=E[l]if not I then I=e(q[k(l,18,6)],q[k(l,12,6)],q[k(l,6,6)],q[k(l,0,6)])E[l]=I end;A[B]=I;B=B+1 end;if D==2 then local F,G=f(z,C-1,C)local l=F*0x10000+G*0x100;A[B]=e(q[k(l,18,6)],q[k(l,12,6)],q[k(l,6,6)],q[64])elseif D==1 then local l=f(z,C)*0x10000;A[B]=e(q[k(l,18,6)],q[k(l,12,6)],q[64],q[64])end;return j(A)end;function base64.decode(J,r)r=v[r or"base64"]or error("invalid alphabet specified",2)local K="[^%w%+%/%=]"if r then local L,M;for N,O in pairs(r)do if O==62 then L=N elseif O==63 then M=N end end;K=i("[^%%w%%%s%%%s%%=]",e(L),e(M))end;J=g(tostring(J),K,'')local E={}local A,B={},1;local C=#J;local P=h(J,-2)=="=="and 2 or h(J,-1)=="="and 1 or 0;for s=1,P>0 and C-4 or C,4 do local F,G,H,Q=f(J,s,s+3)local R=F*0x1000000+G*0x10000+H*0x100+Q;local I=E[R]if not I then local l=r[F]*0x40000+r[G]*0x1000+r[H]*0x40+r[Q]I=e(k(l,16,8),k(l,8,8),k(l,0,8))E[R]=I end;A[B]=I;B=B+1 end;if P==1 then local F,G,H=f(J,C-3,C-1)local l=r[F]*0x40000+r[G]*0x1000+r[H]*0x40;A[B]=e(k(l,16,8),k(l,8,8))elseif P==2 then local F,G=f(J,C-3,C-2)local l=r[F]*0x40000+r[G]*0x1000;A[B]=e(k(l,16,8))end;return j(A)end;return base64; 
end)()

local api = panorama.open().SteamOverlayAPI

local hVisuals = { }
local hAntiaim = { }
local hConfigs = { }
local hCallbacks = { }
local hRage = { }
local hMisc = { }

-- [[ Script data ]] --
local script_tbl = {
    ver = '1.0',
    build = 'release',
    screen = render.get_screen_size(),
}

local dpi_scale = script_tbl.screen.y / 1440 -- pasted from eclipse $$$ (i was too lazy)
if dpi_scale < 1 then
    dpi_scale = 1
end

local s_print = print
print = function(...)
    if not _G._DEBUG then return end
    return s_print(...)
end

-- [[ Tables ig ]] -- 
local states = etr.get_states()
local hitboxes = { [0] = 'generic', 'head', 'chest', 'stomach', 'left arm', 'right arm', 'left leg', 'right leg', 'neck', 'gear' }
local fonts = { 
    ui = render.create_font("Verdana", 12 * dpi_scale, 400, e_font_flags.DROPSHADOW, e_font_flags.ANTIALIAS, e_font_flags.GAUSSIANBLUR),
    crosshair = render.create_font("Verdana", 10 * dpi_scale, 600, e_font_flags.DROPSHADOW, e_font_flags.ANTIALIAS, e_font_flags.GAUSSIANBLUR),
    notifications = render.create_font("Verdana", 13 * dpi_scale, 500, e_font_flags.DROPSHADOW, e_font_flags.ANTIALIAS, e_font_flags.GAUSSIANBLUR),
    title = render.create_font('Verdana', 23 * dpi_scale, 600, e_font_flags.ANTIALIAS, e_font_flags.DROPSHADOW, e_font_flags.GAUSSIANBLUR),
    changelog = render.create_font('Verdana', 15 * dpi_scale, 600, e_font_flags.ANTIALIAS, e_font_flags.GAUSSIANBLUR, e_font_flags.DROPSHADOW),
}

local text = {
    date = '6.9.420 | v1.0',
    rage = {
        'cool rage update #1',
        'cool rage update #2',
        'cool rage update #3',
        'cool rage update #4',
        'cool rage update #5',
    },
    antiaim = {
        'cool antiaim update #1',
        'cool antiaim update #2',
        'cool antiaim update #3',
        'cool antiaim update #4',
        'cool antiaim update #5',
    },
    visuals = {
        'cool visuals update #1',
        'cool visuals update #2',
        'cool visuals update #3',
        'cool visuals update #4',
        'cool visuals update #5',
    },
    misc = {
        'cool misc update #1',
        'cool misc update #2',
        'cool misc update #3',
        'cool misc update #4',
        'cool misc update #5',
    },
    script = {
        'cool script update #1',
        'cool script update #2',
        'cool script update #3',
        'cool script update #4',
        'cool script update #5',
    }
}

local function get_changelog_texts()
    local order = {"rage", "antiaim", "visuals", "misc", "script"} -- order from to go (since its retarded lul)
    local lines = {} -- store the text

    local date = text.date -- get the date
    if date then
        table.insert(lines, "Date: " .. date .. "\n")
    end

    for _, category in pairs(order) do
        local changes = text[category] -- get the text from tables
        if changes and #changes > 0 then -- if empty dont render the category name
            if category ~= "date" then -- if date then skip c:
                table.insert(lines, category .. ":") -- insert the category name
                for _, change in pairs(changes) do -- loop through the individual tables
                    table.insert(lines, "- " .. change) -- render the text inside and add "-"
                end
                table.insert(lines, "") -- skip lines
            end
        end
    end

    local render_text = table.concat(lines, "\n") -- concat all the text

    return render_text -- return it
end

-- [[ References ]] -- (yes i wrote it by hand :sob:)
local menu_find = menu.find
local c_refs = {
    aimbot = {
        doubletap = menu_find("aimbot", "general", "exploits", "doubletap", "enable"),
        hideshots = menu_find("aimbot", "general", "exploits", "hideshots", "enable"),
        autopeek = menu_find("aimbot", "general", "misc", "autopeek"),
        anti_pred_air = menu_find('aimbot', 'general', 'exploits', 'doubletap', 'anti-prediction in air')
    },
    antiaim = {
        enable = menu_find("antiaim",'main', "general", "enable"),
        anti_knife = menu_find("antiaim",'main', "general", "anti knife"),
        fakeduck = menu_find("antiaim",'main', "general", "fakeduck")
    },
    angles = {
        pitch = menu_find("antiaim","main", "angles", "pitch"),
        yaw_base = menu_find("antiaim","main", "angles", "yaw base"),
        yaw_add = menu_find("antiaim","main", "angles", "yaw add"),
        rotate = menu_find("antiaim","main", "angles", "rotate"),
        rotate_range = menu_find("antiaim","main", "angles", "rotate range"),
        rotate_speed = menu_find("antiaim","main", "angles", "rotate speed"),
        jitter_mode = menu_find("antiaim","main", "angles", "jitter mode"),
        jitter_type = menu_find("antiaim","main", "angles", "jitter type"),
        jitter_add = menu_find("antiaim","main", "angles", "jitter add"),
    },
    desync = {
        stand_side = menu_find("antiaim","main", "desync", "side#stand"),
        stand_left_amount = menu_find("antiaim","main", "desync", "left amount#stand"),
        stand_right_amount = menu_find("antiaim","main", "desync", "right amount#stand"),
        move_override = menu_find("antiaim", "main","desync", "override stand#move"),
        slowwalk_override = menu_find("antiaim","main", "desync", "override stand#slow walk"),
        anti_bruteforce = menu_find("antiaim","main", "desync", "anti bruteforce"),
        on_shot = menu_find("antiaim","main", "desync", "on shot"),
    },
    fakelag = {
        fake_lag_value = menu_find("antiaim","main", "fakelag", "amount"),
        fake_lag_break = menu_find("antiaim","main", "fakelag", "break lag compensation"),
    },
}

-- [[ Indicators table ]] --
local indicators_table = {
    {
        state = c_refs.aimbot.doubletap[2],
        name = 'DT',
        cl = color_t(255,255,255),
        alpha = 0
    },
    {
        state = c_refs.aimbot.hideshots[2],
        name = 'OS',
        cl = color_t(255,255,255),
        alpha = 0
    },
    {
        state = true,
        name = 'DMG',
        cl = color_t(255,255,255),
        alpha = 0
    },
    {
        state = true,
        name = 'HC',
        cl = color_t(255,255,255),
        alpha = 0
    },
    {
        state = c_refs.antiaim.fakeduck[2],
        name = 'FD',
        cl = color_t(255,255,255),
        alpha = 0
    },
    {
        state = c_refs.aimbot.autopeek[2],
        name = 'PEEK',
        cl = color_t(255,255,255),
        alpha = 0
    }
}

-- [[ Home UI ]] -- 
local tab_col = ui.create('etrium.dev [' .. script_tbl.build .. ']', 1)
local home_col = ui.create('Home', 2)
local socials_col = ui.create('Socials', 1)
local config_col = ui.create('Config', 3)
local config_col_s = ui.create('import/export', 2)

local tabs = tab_col:selection('Choose tab', {'Home', 'Rage', 'Anti-Aim', 'Visuals', 'Miscellaneous'}, 5)
        
home_col:text('> Welcome to etrium.dev')
home_col:text('> Registered to ' .. user.name .. ' ['.. user.uid ..']')
home_col:text('> Build : ' .. script_tbl.build)
local sesion_time_text = home_col:text('> Session time : ' .. '0s')
local hide_change = home_col:checkbox('Hide changelogs')

-- [[ Socials UI ]] --
socials_col:button('Discord', function() api.OpenExternalBrowserURL('https://discord.gg/d47yjy9ce5') end)
socials_col:button('Website', function() api.OpenExternalBrowserURL('https://etrium.dev') end)
socials_col:button('Youtube', function() api.OpenExternalBrowserURL('https://youtube.com/danixhvh') end) 

-- [[ Rage UI ]]
local rage_col = ui.create('Rage', 2)
local baim_enm_air = rage_col:checkbox('Baim in air enemies')
local baim_slider_ms = rage_col:slider('When activate',0, 1, 0.1, 1, ' s')
baim_slider_ms.ref:set(0.2)

-- [[ Antiaim UI ]]
local subtab_antiaim = ui.create('Subtabs', 1)
local sub_tabs = subtab_antiaim:list('Select subtab', {'Main', 'Exploits', 'Antibrute', 'Other'}, 4)
local aa_conditions = subtab_antiaim:selection('Condition', states, 7)

local antiaim_main = ui.create('Main', 2)
local antiaim_brute = ui.create('Anti-Bruteforce', 2)
local antiaim_other_p = ui.create('Options', 2)

local antiaim_base = antiaim_main:selection('Base settings', {'Aggressive' --[[, 'Defensive']]})
antiaim_main:separator()

local ab_enabled = antiaim_brute:checkbox('Enable', false)
local ab_timer = antiaim_brute:slider('Reset timer', 0, 30, 1, 0, ' sec')

local breakers = antiaim_other_p:multi_selection('Anim. breakers', {'0 pitch on land', 'Static legs in air', 'Walk in air', 'Lean in air'})
local disablers = antiaim_other_p:multi_selection('Jitter disablers', {'Manual', 'Freestand'})

local antiaim_col = { }
local desync_col = { }
local fakelag_col = { }
local defensive_col = { }
local defensive_col2 = { }

for i, v in pairs(states) do
    antiaim_main:checkbox('Override ' .. v .. ' condition', false)

    antiaim_col[v] = ui.create('Angles [' .. v .. ']', 2)
    desync_col[v] = ui.create('Desync [' .. v .. ']', 3)
    fakelag_col[v] = ui.create('Fakelag [' .. v .. ']', 3)
    defensive_col[v] = ui.create('Defensive [' .. v .. ']', 2)
    defensive_col2[v] = ui.create('Options [' .. v .. ']', 3)

    antiaim_col[v]:selection('Pitch', {'None', 'Down', 'Up', 'Zero', 'Jitter', 'Custom'})
    antiaim_col[v]:slider('Custom pitch', -89, 89)
    antiaim_col[v]:selection('Yaw', {'None', 'Viewangle', 'Crosshair', 'Distance', 'Velocity'})
    antiaim_col[v]:slider('Yaw add', -180, 180)
    antiaim_col[v]:selection('Jitter mode', {'None', 'Static', 'Random'})
    antiaim_col[v]:selection('Jitter type', {'Offset', 'Center', '3-Way', '5-Way', 'Delayed'})
    antiaim_col[v]:slider('Delay jitter', 0, 64, 1, 0, ' tick')
    antiaim_col[v]:slider('Jitter add', -180, 180)

    desync_col[v]:selection('Desync side', {'None', 'Left', 'Right', 'Jitter', 'Peek fake', 'Peek real', 'Sway'}, 7)
    desync_col[v]:slider('Left', 0, 100)
    desync_col[v]:slider('Right', 0, 100)
    desync_col[v]:selection('On shot', {'Off', 'Opposite', 'Same side', 'Random'})

    fakelag_col[v]:slider('Fakelag', 0, 15)
    fakelag_col[v]:checkbox('Break LC')

    defensive_col[v]:checkbox('Enable')
    defensive_col[v]:selection('Trigger', {'On peek', 'Always'}, 2)

    defensive_col2[v]:selection('Pitch', {'None', 'Down', 'Up', 'Zero', 'Jitter', 'Custom'})
    defensive_col2[v]:slider('Custom pitch', -89, 89)
    defensive_col2[v]:slider('Yaw offset', 0, 180)
    defensive_col2[v]:checkbox('Spin')
    defensive_col2[v]:checkbox('Anti-preditct')
end

-- [[ Visuals UI ]] --
local visuals_col = ui.create('Indicators', 2)
local visuals_col2 = ui.create('Elements', 3)
local crosshair_ind = visuals_col:checkbox('Crosshair indicators')
local crosshair_ind_opt = visuals_col:multi_selection('Options', {'Animation speed', 'Slowed', 'Scoped anim.'}, 3)
local anim_speed = visuals_col:slider('Anim. speed', 0, 5)
local cross_color = visuals_col:color_picker(visuals_col['Crosshair indicators'], 'cross color', color_t(189, 157, 245))
local defensive_ind = visuals_col:checkbox('Defensive indicator')
local defensive_color = visuals_col:color_picker(visuals_col['Defensive indicator'], 'defensive color', color_t(189, 157, 245))
local notifications = visuals_col:multi_selection('Notifications', {'Hit', 'Miss', 'Antibrute'}, 3)
local notifications_time = visuals_col:slider('Time', 0, 10, 1, 0, 's')
notifications_time.ref:set(5)
local notifications_col = visuals_col:color_picker(visuals_col['Notifications'], 'bar color', color_t(189, 157, 245))
local prefix_col = visuals_col:color_picker(visuals_col['Notifications'], 'prefix color', color_t(189, 157, 245))
local Elements_selection = visuals_col2:multi_selection('UI elements', {'Watermark', 'Spectators', 'Keybinds'}, 3)
local elements_col = visuals_col2:color_picker(visuals_col2['UI elements'], 'ui color', color_t(189, 157, 245))
local watermark_selection = visuals_col2:multi_selection('Watermark selection', {'Name', 'Ping', 'Fps', 'Time'}, 4)
local watermark_time_format = visuals_col2:checkbox('Change time format')

--[[ MISC ]] --
local misc_col = ui.create('Misc', 2)
local clantag_toggle = misc_col:checkbox('Clantag')
local killsay_toggle = misc_col:checkbox('Kill say')
local killsay_type = misc_col:selection('Type', {'Retarded', 'Ad'}, 2)
local killsay_delay = misc_col:slider('Delay', 0, 5, 1, 0, 's')

-- [[ Functions ]] --
local start_time = globals.real_time()

local sesion_time_Updater = function()
    local time = globals.real_time() - start_time
    local seconds = math.floor(time % 60)
    local minutes = math.floor((time / 60) % 60)
    local hours = math.floor(time / 3600)

    if hours >= 1 then
        sesion_time_text.ref:set(string.format("> Session time : %dh, %dmin, %dsec", hours, minutes, seconds))
    elseif minutes >= 1 then
        sesion_time_text.ref:set(string.format("> Session time : %dmin, %dsec", minutes, seconds))
    else
        sesion_time_text.ref:set(string.format("> Session time : %dsec", seconds))
    end
end

local visibility_func = function()
    if not menu.is_open() then return end
    home_col:depend({tabs, 1})
    socials_col:depend({tabs, 1})
    config_col:depend({tabs, 1})
    config_col_s:depend({tabs, 1})

    rage_col:depend({tabs, 2})

    misc_col:depend({tabs, 5})
    
    subtab_antiaim:depend({tabs, 3})
    antiaim_main:depend({tabs, 3}, {sub_tabs, 1})
    antiaim_brute:depend({tabs, 3}, {sub_tabs, 3})

    antiaim_other_p:depend({tabs, 3}, {sub_tabs, 4})

    for i, v in pairs(states) do
        antiaim_main['Override ' .. v .. ' condition']:depend({aa_conditions, i})
        
        local should_override = antiaim_main['Override ' .. v .. ' condition']
        
        local aa_tbl = antiaim_col[v]
        local ds_tbl = desync_col[v]
        local df_tbl = defensive_col[v]
        local df2_tbl = defensive_col2[v]

        aa_tbl:depend({tabs, 3}, {sub_tabs, 1}, {aa_conditions, i}, {should_override, true})
        ds_tbl:depend({tabs, 3}, {sub_tabs, 1}, {aa_conditions, i}, {should_override, true})
        fakelag_col[v]:depend({tabs, 3}, {sub_tabs, 1}, {aa_conditions, i}, {should_override, true})

        df_tbl:depend({tabs, 3}, {sub_tabs, 2}, {aa_conditions, i})
        df2_tbl:depend({tabs, 3}, {sub_tabs, 2}, {aa_conditions, i})

        aa_tbl['Custom pitch']:depend({aa_tbl['Pitch'], 6})
        aa_tbl['Yaw add']:depend({(aa_tbl['Yaw']:get() > 1)})
        aa_tbl['Jitter type']:depend({(aa_tbl['Jitter mode']:get() > 1)})
        aa_tbl['Delay jitter']:depend({aa_tbl['Jitter type'], 5})
        aa_tbl['Jitter add']:depend({(aa_tbl['Jitter mode']:get() > 1)})
        ds_tbl['Left']:depend({(ds_tbl['Desync side']:get() ~= 1)})
        ds_tbl['Right']:depend({(ds_tbl['Desync side']:get() ~= 1)})
        ds_tbl['On shot']:depend({(ds_tbl['Desync side']:get() ~= 1)})
        df_tbl['Trigger']:depend({df_tbl['Enable'], true})
        df2_tbl['Pitch']:depend({df_tbl['Enable'], true})
        df2_tbl['Custom pitch']:depend({(df2_tbl['Pitch']:get() == 6)}, {df_tbl['Enable'], true})
        df2_tbl['Yaw offset']:depend({df_tbl['Enable'], true})
        df2_tbl['Anti-preditct']:depend({df_tbl['Enable'], true})
        df2_tbl['Spin']:depend({df_tbl['Enable'], true})
    end

    visuals_col:depend({tabs, 4})
    visuals_col2:depend({tabs, 4})
    crosshair_ind_opt:depend({crosshair_ind, true})
    anim_speed:depend({(crosshair_ind_opt:get('Animation speed') == true)}, {crosshair_ind, true})
    cross_color:depend({crosshair_ind, true})
    defensive_color:depend({defensive_ind, true})
    notifications_time:depend({(notifications:get('Hit') == true or notifications:get('Miss') == true or notifications:get('Antibrute') == true)})

    watermark_selection:depend({(Elements_selection.ref:get('Watermark'))})
    watermark_time_format:depend({(Elements_selection.ref:get('Watermark'))}, {(watermark_selection.ref:get('Time'))})

    killsay_delay:depend({(killsay_toggle.ref:get())})
    killsay_type:depend({(killsay_toggle.ref:get())})
end

local is_active = false
hRage.baim_in_air_enm = function(ctx)
    if not baim_enm_air.ref:get() then return end
    client.delay_call(function() is_active = true end, baim_slider_ms.ref:get())
    if not ctx.player or not ctx.player:get_active_weapon() then return end
    local pl = ctx.player:get_active_weapon():get_name()
    if not pl then return end
    local lp = entity_list.get_local_player()
    if not lp:is_valid() or not lp:is_alive() or lp == nil then return end
    local wep = lp:get_active_weapon():get_name()
    if not wep then return end

    local player = ctx.player
    if player == nil or not player or not player:is_alive() then return end
    local flags = player:get_prop("m_fFlags")
    local inAir = bit.band(flags, bit.lshift(1, 0)) == 0
    if is_active == true and (pl == 'knife' or pl == 'zeus') and wep == 'ssg08' and inAir then
        if player:get_prop("m_iHealth") < 50 then
            ctx:set_min_dmg(50)
        else
            ctx:set_min_dmg(90)
        end
        ctx:set_hitscan_group_state(e_hitscan_groups.HEAD, false) 
        is_active = false
    end
end

-- [[ Antiaim ]] --
local anti_brute = { }
local ab_values = {
    0.5, 0.8, 1.4, 1.7
}
local last_switch_tick = 0
hAntiaim.bruteforce = function(e)
    if not ab_enabled.ref:get() then return end
    if e.name == "bullet_impact" then
        local rndm = math.random(1, #ab_values)
        local random_index = ab_values[rndm]
        local local_player = entity_list.get_local_player()
        if not local_player or not local_player:is_valid() or not local_player:is_alive() or local_player == nil then return end
        if not local_player or e.userid == nil then return end

        local entity = entity_list.get_player_from_userid(e.userid)
        if entity == nil or not entity:is_valid() or not entity:is_alive() or entity:is_dormant() or not entity:is_player() or not entity:is_enemy() then return end
    
        local bullet_impact = vec3_t(e.x, e.y, e.z)
        local eye_pos = entity:get_eye_position()
        if not eye_pos then return end
    
        local local_eye_pos = local_player:get_eye_position()
        if not local_eye_pos then return end
    
        local distance_between = etr.dist_to(etr.closest_point_on_ray(eye_pos, bullet_impact, local_eye_pos), local_eye_pos)
        if distance_between < 105 and global_vars.tick_count() > last_switch_tick + 5 then
            anti_brute.timer = ab_timer.ref:get()
            anti_brute.working = true
            anti_brute.value = random_index
            last_switch_tick = global_vars.tick_count()
            if notifications.ref:get('Antibrute') then
                hVisuals.notify('Switched angle [' .. rndm ..']')
            end
        end
    end
    if e.name == "round_start" then
        anti_brute.timer = 0
        anti_brute.value = 0
        anti_brute.working = false
        last_switch_tick = global_vars.tick_count()
    end
    return anti_brute
end

hAntiaim.abrute_pred = function(ctx)
    if not anti_brute.working then return end

    anti_brute.timer = anti_brute.timer - global_vars.interval_per_tick()

    if anti_brute.timer < 0 then anti_brute.timer = 0 end
    if anti_brute.timer == 0 and anti_brute.working then if notifications.ref:get('Antibrute') then hVisuals.notify('Reseted angle') end anti_brute.working = false end
end

-- default vals
local commonValues = {
    pitch = 2,
    yaw_base = 3,
    yaw_add = 0,
    jitter_mode = 2,
    jitter_type = 3,
    jitter_add = -19,
    desync_side = 2,
    desync_left = 100,
    desync_right = 100,
    desync_onshot = 3,
    fakelag_factor = 14,
    fakelag_break = true
}

hAntiaim.Aggressive_data = {
    ['Stand'] = {},
    ['Crouch'] = {},
    ['Slow walk'] = {},
    ['Run'] = {},
    ['Air'] = {},
    ['Air duck'] = {},
    ['Use'] = {}
}

-- apply common vals to states
for _, data in pairs(hAntiaim.Aggressive_data) do
    for key, value in pairs(commonValues) do
        data[key] = value
    end
end

-- override state table (ex. use)
local override_state = {
    pitch = 1,
    yaw_base = 1,
    yaw_add = 0,
    jitter_mode = 1,
    jitter_type = 2,
    jitter_add = 0,
    desync_side = 5,
    desync_left = 100,
    desync_right = 100,
    desync_onshot = 4,
    fakelag_factor = 10,
    fakelag_break = true
}

-- apply overrided values to states
for key, value in pairs(override_state) do
    hAntiaim.Aggressive_data['Use'][key] = value
end

hAntiaim.get_values = function(state)
    local should_override = antiaim_main['Override ' .. state .. ' condition']:get()
    
    -- Cache the tables LoL
    local aa_tbl = antiaim_col[state]
    local ds_tbl = desync_col[state]
    local fl_tbl = fakelag_col[state]
    
    -- Base builder
    local pitch = aa_tbl['Pitch']:get()
    local yaw_base = aa_tbl['Yaw']:get()
    local yaw_add = aa_tbl['Yaw add']:get()
    local jitter_mode = aa_tbl['Jitter mode']:get()
    local jitter_type = aa_tbl['Jitter type']:get()
    local jitter_add = aa_tbl['Jitter add']:get()               
    local desync_side = ds_tbl['Desync side']:get()
    local desync_left = ds_tbl['Left']:get()
    local desync_right = ds_tbl['Right']:get()
    local desync_onshot = ds_tbl['On shot']:get()
    local fakelag_factor = fl_tbl['Fakelag']:get()
    local fakelag_break = fl_tbl['Break LC']:get()

    -- Extended builder
    local custom_pitch = aa_tbl['Custom pitch']:get()
    local delay_jitter = aa_tbl['Delay jitter']:get()

    -- Base settings
    local aa_data = hAntiaim.Aggressive_data[state]
    local data = {
        -- Base builder
        pitch = should_override and pitch or aa_data.pitch,
        yaw_base = should_override and yaw_base or aa_data.yaw_base,
        yaw_add = should_override and yaw_add or aa_data.yaw_add,
        jitter_mode = should_override and jitter_mode or aa_data.jitter_mode,
        jitter_type = should_override and jitter_type or aa_data.jitter_type,
        jitter_add = should_override and jitter_add or aa_data.jitter_add,
        desync_side = should_override and desync_side or aa_data.desync_side,
        desync_left = should_override and desync_left or aa_data.desync_left,
        desync_right = should_override and desync_right or aa_data.desync_right,
        desync_onshot = should_override and desync_onshot or aa_data.desync_onshot,
        fakelag_factor = should_override and fakelag_factor or aa_data.fakelag_factor,
        fakelag_break = should_override and fakelag_break or aa_data.fakelag_break,

        -- Extended builder
        custom_pitch = custom_pitch,
        delay_jitter = delay_jitter
    }

    return data
end

hAntiaim.set_values = function(data)
    c_refs.angles.pitch:set(data.pitch)
    c_refs.angles.yaw_base:set(data.yaw_base)
    c_refs.angles.yaw_add:set(data.yaw_add)
    c_refs.angles.jitter_mode:set(data.jitter_mode)
    c_refs.angles.jitter_type:set(data.jitter_type)
    if (disablers.ref:get('Freestand') and ragebot.get_autopeek_pos() ~= nil) or (disablers.ref:get('Manual') and antiaim.get_manual_override() > 0) then
        c_refs.angles.jitter_add:set(0)
    else
        c_refs.angles.jitter_add:set(anti_brute.working and data.jitter_add*anti_brute.value or data.jitter_add)
    end
    c_refs.desync.stand_side:set(data.desync_side)
    c_refs.desync.stand_left_amount:set(data.desync_left)
    c_refs.desync.stand_right_amount:set(data.desync_right)
    c_refs.desync.on_shot:set(data.desync_onshot)
    c_refs.fakelag.fake_lag_value:set(data.fakelag_factor)
    c_refs.fakelag.fake_lag_break:set(data.fakelag_break)
end

hAntiaim.defensive = function(ctx, cmd, unpred, lp, data)
    local state = etr.get_current_state(lp)

    local def_c = defensive_col[state]
    local def_c2 = defensive_col2[state]

    local toggle = def_c['Enable']:get()
    if not toggle then c_refs.angles.rotate:set(false) return end
    local trigger = def_c['Trigger']:get()

    local def_pitch = def_c2['Pitch']:get()
    local custom_p = def_c2['Custom pitch']:get()
    local offset = def_c2['Yaw offset']:get()
    local antipred = def_c2['Anti-preditct']:get()
    local spin = def_c2['Spin']:get()

    local realtime = math.floor(globals.real_time() * 6000)
    local is_defensive = etr.is_defensive(lp)

    if trigger == 2 then exploits.force_anti_exploit_shift() end

    if spin and is_defensive then
        c_refs.angles.rotate:set(true)
        c_refs.angles.rotate_range:set(360)
        c_refs.angles.rotate_speed:set(100)
    elseif not is_defensive then
        c_refs.angles.rotate:set(false)
    end

    local side = realtime % 2 == 0 and offset or -offset
    if is_defensive then
        if antipred then
            c_refs.aimbot.anti_pred_air:set(true)
            if realtime % 6 > math.random(1,2) then
                if def_pitch ~= 6 then data.pitch = def_pitch else ctx:set_pitch(custom_p) end
                data.yaw_add = side
            end
        else
            c_refs.aimbot.anti_pred_air:set(false)
            if def_pitch ~= 6 then data.pitch = def_pitch else ctx:set_pitch(custom_p) end
            data.yaw_add = side
        end
    end
end

local counter = 0
local cache = 0
hAntiaim.handle = function(ctx, cmd, unpred)
    if not engine.is_connected() or not engine.is_in_game() then return end
    local lp = entity_list.get_local_player()
    if lp == nil or not lp:is_valid() or not lp:is_alive() then return end

    local state = etr.get_current_state(lp)
    local data = hAntiaim.get_values(state)

    c_refs.desync.move_override:set(false)
    c_refs.desync.slowwalk_override:set(false)
    --c_refs.desync.anti_bruteforce:set(false)
    --c_refs.angles.rotate:set(false)

    local jitter = 0
    if data.jitter_type == 5 then
        if (disablers.ref:get('Freestand') and ragebot.get_autopeek_pos() ~= nil) or (disablers.ref:get('Manual') and antiaim.get_manual_override() > 0) then goto skip end
        local val = etr.set_jitter('delayed', data.delay_jitter) == 0
        local j_m = data.jitter_mode ~= 3 and data.jitter_add or math.random(-data.jitter_add, data.jitter_add)

        -- Save up the memory :)
        if counter > 1500 then counter = 0 end

        if counter % 2 == 0 and data.desync_side == 4 then 
            data.desync_left = 0
            data.desync_right = 0
            ctx:set_desync(1.0)
        elseif counter % 2 ~= 0 and data.desync_side == 4 then
            data.desync_right = 0
            data.desync_left = 0
            ctx:set_desync(-1.0)
        end
        if val then
            counter = counter + 1
            jitter = counter % 2 == 0 and math.floor((anti_brute.working and j_m*anti_brute.value or j_m)) or math.floor((anti_brute.working and (-j_m)*anti_brute.value or -j_m))
            data.yaw_add = data.yaw_add + jitter
            cache = data.yaw_add
        else
            data.yaw_add = cache
        end
        ::skip::
    end

    if data.pitch == 6 then
        data.pitch = 1
        ctx:set_pitch(data.custom_pitch)
    end

    hAntiaim.defensive(ctx, cmd, unpred, lp, data)
    hAntiaim.set_values(data)
end

hAntiaim.groundTick = 1
hAntiaim.endTime = 0
hAntiaim.breakers = function(ctx)
    local local_player = entity_list.get_local_player()
    if not local_player or not local_player:is_valid() or not local_player:is_alive() then return end
    
    local flags = local_player:get_prop("m_fFlags")
    local land = bit.band(flags, bit.lshift(1, 0)) ~= 0
    local inAir = bit.band(flags, bit.lshift(1, 0)) == 0
    local curTime = global_vars.cur_time()

    if land then 
        hAntiaim.groundTick = hAntiaim.groundTick + 1
    else
        hAntiaim.groundTick = 0
        hAntiaim.endTime = curTime + 1
    end

    if breakers.ref:get(1) and hAntiaim.groundTick > 1 then
        if hAntiaim.endTime > curTime then
            ctx:set_render_pose(e_poses.BODY_PITCH, 0.5)
        end
    end

    if breakers.ref:get(2) and inAir then
        ctx:set_render_pose(e_poses.JUMP_FALL, 1) 
    end

    if breakers.ref:get(3) and inAir then
        ctx:set_render_animlayer(e_animlayers.MOVEMENT_MOVE, 1)
    end

    if breakers.ref:get(4) and inAir then
        ctx:set_render_animlayer(e_animlayers.LEAN, 1)
    end
end

-- [[ Visuals ]] --
hVisuals.text_size = render.get_text_size(fonts.ui, "Defensive") -- so it scales with dpi + also centers the bar with the text width
hVisuals.middle_size = hVisuals.text_size.x / 2
hVisuals.time = hVisuals.text_size.x
local def_drag = etr.new('defensive', vec2_t((script_tbl.screen.x / 2) - (hVisuals.middle_size), script_tbl.screen.y / 4 - 15), vec2_t(hVisuals.text_size.x, 20))
hVisuals.defensive_ind = function()
    if not defensive_ind.ref:get() then return end

    local lp = entity_list.get_local_player()
    if lp == nil or not lp:is_valid() or not lp:is_alive() then return end
    local pos = def_drag.pos
    local size = def_drag.size
    def_drag:update()
    local is_defensive = etr.is_defensive(lp)

    hVisuals.time = etr.lerp(hVisuals.time, 0, 10)

    local alpha = menu.is_open() and 1 or hVisuals.time / hVisuals.text_size.x

    if is_defensive or menu.is_open() then
        render.push_alpha_modifier(alpha)
        render.text(fonts.ui, "Defensive", pos, color_t(255, 255, 255, 255))
        render.rect_filled(vec2_t(pos.x, pos.y + 16), vec2_t(hVisuals.text_size.x, 7), color_t(0, 0, 0), 5)
        render.rect_filled(vec2_t(pos.x, pos.y + 16), vec2_t(math.floor(menu.is_open() and hVisuals.middle_size or hVisuals.time), 5), defensive_color.ref:get(), 5)
        render.pop_alpha_modifier()
    else
        hVisuals.time = hVisuals.text_size.x
    end
end

hVisuals.scoped_offset = 0
local offset2 = 25
local ind_drag = etr.new('indicators', vec2_t(script_tbl.screen.x / 2 - (render.get_text_size(fonts.ui, 'etrium.dev').x / 2), script_tbl.screen.y / 2 + 5), vec2_t(0, 0))
hVisuals.crosshair_indicator = function()
    if not crosshair_ind.ref:get() then return end
    if not engine.is_connected() or not engine.is_in_game() then return end

    local lp = entity_list.get_local_player()
    if lp == nil or not lp:is_valid() or not lp:is_alive() then return end
    local offset = 0

    local ind_pos = ind_drag.pos
    local ind_size = ind_drag.size

    local opt = crosshair_ind_opt:get('Slowed')
    local opt2 = crosshair_ind_opt:get('Scoped anim.')
    local screen = script_tbl.screen
    local scoped = lp:get_prop("m_bIsScoped")
    hVisuals.scoped_offset = (scoped == 1 and opt2) and etr.lerp(hVisuals.scoped_offset, 40, 5) or etr.lerp(hVisuals.scoped_offset, 0, 9)
    local font = fonts.ui
    local text = etr.animated_text(font, 'etrium.dev', cross_color.ref:get(), ind_pos.x + hVisuals.scoped_offset + (ind_size.x / 2), ind_pos.y, anim_speed.ref:get())
    text()
    indicators_table[3].state = menu_find("aimbot", etr.active_weapon(), "target overrides", "min. damage")[2]
    indicators_table[4].state = menu_find("aimbot", etr.active_weapon(), "target overrides", "hitchance")[2]

    ind_drag:update()
    ind_drag:lerp("size", vec2_t(render.get_text_size(font, 'etrium.dev').x, 10), 5)

    local velocity = 1
    velocity = lp:get_prop('m_flVelocityModifier')

    if opt then
        offset2 = (velocity < 1) and etr.lerp(offset2, 33, 10) or etr.lerp(offset2, 25, 7)
        if menu.is_open() then goto skip end
        local alpha_modifier = (offset2 - 25) / (33 - 25)

        render.push_alpha_modifier(alpha_modifier)
        render.rect_filled(vec2_t(ind_pos.x + hVisuals.scoped_offset - (45/2) + (ind_size.x / 2), ind_pos.y + 15), vec2_t(45, 6), color_t(5,5,5,255))
        render.rect_fade(vec2_t(ind_pos.x + hVisuals.scoped_offset - (43/2) + (ind_size.x / 2), ind_pos.y + 16), vec2_t(43*velocity, 4), cross_color.ref:get(), color_t(5,5,5,255), true)
        render.pop_alpha_modifier()
        ::skip::
    end

    for i,v in pairs(indicators_table) do
        if menu.is_open() then goto skip end
        if not v.state:get() then v.alpha = v.alpha <= 0 and 0 or etr.lerp(v.alpha, -1, 8) if v.alpha <= 0 then goto skip end end
        if v.state:get() then v.alpha = etr.lerp(v.alpha, 1, 8) end
        local center_text = render.get_text_size(fonts.crosshair, v.name).x * 0.5
        local col = v.cl
        render.text(fonts.crosshair, v.name, vec2_t(ind_pos.x + hVisuals.scoped_offset - center_text + (ind_size.x / 2), ind_pos.y + offset2 + offset - 10), etr.color_anim(col.r, col.g, col.b, v.alpha * 255))
        offset = offset + math.floor(9 * v.alpha + 0.7)

        ::skip::
    end
end

hVisuals.logs = { }

hVisuals.notify = function(txt)
    if #hVisuals.logs > 5 then table.remove(hVisuals.logs, 1) end
    table.insert(hVisuals.logs, {
        text = txt, 
        alpha = 0,
        y = 0,
        time = globals.real_time() + notifications_time.ref:get(), 
    })
end

hVisuals.noti_time = 0
hVisuals.notifications = function()
    if #hVisuals.logs <= 0 then return end

    local offset = 0
    local screen = script_tbl.screen
    local notifications_time = notifications_time.ref:get()

    for i, v in pairs(hVisuals.logs) do
        local timer = v.time - globals.real_time()

        local s_t = render.get_text_size(fonts.notifications, v.text)
        local anim_text_offset = render.get_text_size(fonts.notifications, 'etrium')

        hVisuals.noti_time = etr.clamp(10 * (timer / notifications_time), 0, 10)
        v.alpha = etr.lerp(v.alpha, timer < 0.2 and 0 or 255, 12)
        v.y = etr.lerp(v.y, 25, 12)

        if timer < 0.2 and v.alpha < 10 then
            offset = offset - v.y
            table.remove(hVisuals.logs, i)
            break
        else
            offset = offset + v.y

            local notif_width = s_t.x + anim_text_offset.x + 15

            local alpha = math.floor(v.alpha)
            local p_x, p_y = screen.x / 2 - (notif_width / 2), screen.y / 1.3 - offset * (v.alpha / 255)

            local clr = notifications_col.ref:get()
            local clr2 = prefix_col.ref:get()
            local prefix = etr.animated_text(fonts.notifications, 'etrium ', color_t(clr2.r, clr2.g, clr2.b, alpha), p_x + 27, p_y + 5, 1)
            local glow_alpha = timer < 0.5 and 0 or 50

            render.rect_filled(vec2_t(p_x, p_y + 3), vec2_t(notif_width, 20), color_t(15, 15, 15, alpha), 5) -- bg

            render.rect_filled(vec2_t(p_x + 2, p_y + 8), vec2_t(2, hVisuals.noti_time), color_t(clr.r, clr.g, clr.b, alpha)) -- left line
            render.rect_filled(vec2_t(p_x + notif_width - 4, p_y + 8), vec2_t(2, hVisuals.noti_time), color_t(clr.r, clr.g, clr.b, alpha)) -- right line

            etr.glow(vec2_t(p_x + notif_width - 4, p_y + 8), vec2_t(2, hVisuals.noti_time), 5, color_t(clr.r, clr.g, clr.b, glow_alpha), 10) -- right glow
            etr.glow(vec2_t(p_x + 2, p_y + 8), vec2_t(2, hVisuals.noti_time), 5, color_t(clr.r, clr.g, clr.b, glow_alpha), 10) -- left glow

            prefix()
            render.text(fonts.notifications, v.text, vec2_t(p_x + anim_text_offset.x + 10, p_y + 5), color_t(255, 255, 255, alpha))
        end
    end
end

hVisuals.hitlogs = function(hit)
    if not notifications.ref:get('Hit') then return end
    local text = ('Hit %s at %s [%s] for %ihp [%ihp]'):format(hit.player:get_name(), hitboxes[hit.hitgroup], hitboxes[hit.aim_hitgroup], hit.damage, hit.aim_damage)
    hVisuals.notify(text)
end

hVisuals.misslogs = function(miss)
    if not notifications.ref:get('Miss') then return end
    local text = ('Missed %s due to %s'):format(miss.player:get_name(), miss.reason_string)
    hVisuals.notify(text)
end

hVisuals.alpha_menu = 0
hVisuals.size_y = 0
hVisuals.size_x = 0
hVisuals.changelogs = function()
    if hide_change.ref:get() then return end
    local size = menu.get_size()
    local pos = menu.get_pos()
    local color = menu.find('misc','main','personalization','accent color')[2]:get()
    local text_size = render.get_text_size(fonts.changelog, get_changelog_texts())
    
    hVisuals.alpha_menu = menu.is_open() and etr.lerp(hVisuals.alpha_menu, 255, 10) or etr.lerp(hVisuals.alpha_menu, 0, 10)
    if hVisuals.alpha_menu > 253 then hVisuals.alpha_menu = 255 end -- lerp would get stuck at 254
    if hVisuals.alpha_menu == 0 then return end
    
    if size.y < text_size.y + 70 then -- 1000iq danix hvh bo$$
        hVisuals.size_y = text_size.y + 70 
    else
        hVisuals.size_y = size.y
    end

    if text_size.x + 20 > 350 then
        hVisuals.size_x = text_size.x + 20
    else
        hVisuals.size_x = 355
    end

    render.rect_filled(pos - vec2_t(hVisuals.size_x,0), vec2_t(hVisuals.size_x - 5,hVisuals.size_y), color_t(35,35,35,math.floor(hVisuals.alpha_menu)), 10) -- render bg
    render.rect_filled(pos - vec2_t(hVisuals.size_x,-54), vec2_t(hVisuals.size_x - 5,1), color_t(color.r, color.g, color.b, math.floor(hVisuals.alpha_menu)), 10) -- render line
    render.text(fonts.title, 'etrium.dev', pos - vec2_t(hVisuals.size_x - 10,-15), color_t(color.r, color.g, color.b, math.floor(hVisuals.alpha_menu))) -- render title
    render.text(fonts.changelog, get_changelog_texts(), pos - vec2_t(hVisuals.size_x - 10, -60), color_t(255, 255, 255, math.floor(hVisuals.alpha_menu))) -- render changelogs
end

local watermark_drag = etr.new('watermark', vec2_t(10,10), vec2_t(0,0))

hVisuals.fps = 0
hVisuals.s_x = 0
hVisuals.lerp_ping = 0
hVisuals.watermark = function()
    if not Elements_selection.ref:get('Watermark') then return end

    local things_tbl = { 'etrium.dev' }
    local things = watermark_selection.ref
    hVisuals.fps = etr.lerp(hVisuals.fps, client.get_fps(), 5)
    local ping = math.floor(1000 * engine.get_latency(e_latency_flows.OUTGOING) + engine.get_latency(e_latency_flows.INCOMING))
    hVisuals.lerp_ping = etr.lerp(hVisuals.lerp_ping, ping, 5)

    if things:get('Name') then
        table.insert(things_tbl, (' | %s'):format(user.name))
    end

    if things:get('Ping') then
        table.insert(things_tbl, (' | %ims'):format(hVisuals.lerp_ping))
    end
    
    if things:get('Fps') then
        table.insert(things_tbl, (' | %i fps'):format(hVisuals.fps))
    end
    
    if things:get('Time') then
        local h, m, s = client.get_local_time()
        if m < 10 then m = '0' .. m end
        if h < 10 then h = '0' .. h end

        if watermark_time_format.ref:get() then
            local period = "AM"
            if tonumber(h) >= 12 then
                period = "PM"
                if tonumber(h) > 12 then
                    h = tonumber(h) - 12
                end
            end

            if tonumber(h) == 0 then
                h = 12
            end

            local ampmTime = ('%02d:%02d %s'):format(h, m, period)
            table.insert(things_tbl, (' | %s'):format(ampmTime))
        else
            local militaryTime = ('%s:%s'):format(h, m)
            table.insert(things_tbl, (' | %s'):format(militaryTime))
        end
    end
    
    local text = table.concat(things_tbl)
    local text_size = render.get_text_size(fonts.notifications, text)
    watermark_drag:update()
    watermark_drag:lerp("size", vec2_t(text_size.x + 15, 20), 5)
    local col = elements_col.ref:get()
    local pos = watermark_drag.pos
    local size = watermark_drag.size

    render.rect_filled(pos, size, color_t(15, 15, 15, 255), 5) -- bg

    render.rect_filled(vec2_t(pos.x, pos.y + (size.y/2) - 5), vec2_t(2, 10), color_t(col.r,col.g,col.b)) -- line left
    etr.glow(vec2_t(pos.x, pos.y + (size.y/2) - 5), vec2_t(2, 10), 5, color_t(col.r,col.g,col.b, 50), 10)

    render.rect_filled(vec2_t(pos.x + size.x - 2, pos.y + (size.y/2) - 5), vec2_t(2, 10), color_t(col.r,col.g,col.b)) -- line right
    etr.glow(vec2_t(pos.x + size.x - 2, pos.y + (size.y/2) - 5), vec2_t(2, 10), 5, color_t(col.r,col.g,col.b, 50), 10)

    render.text(fonts.notifications, text, vec2_t(pos.x + (size.x/2), pos.y + (size.y/2)), color_t(255, 255, 255, 255), true)
end

hVisuals.mode = {
    [0] = 'Toggle', 'Hold', 'Hold off', 'Always on', 'Always off'
}

hVisuals.unsorted = { 
    -- aimbot
    ['Quick-peek'] = menu.find('aimbot','general','misc','autopeek')[2],
    ['Rapid-shot'] = menu.find("aimbot", "general", "exploits", "doubletap", "enable")[2],
    ['On-shot antiaim'] = menu.find("aimbot", "general", "exploits", "hideshots", "enable")[2],
    ['Force recharge'] = menu.find("aimbot", "general", "exploits", "instant recharge")[2],
    ['Fake ping'] = menu.find("aimbot", "general", "fake ping", "enable")[2],
    ['Dormant aimbot'] = menu.find("aimbot", "general", "dormant aimbot", "enable")[2],
    -- weapons
    ['Lethal shot'] = false,
    ['Min. damage'] = false,
    ['Hitbox override'] = false,
    ['Safepoint'] = false,
    ['Lean safepoint'] = false,
    ['Hitchance ovr.'] = false,
    -- antiaim
    ['Fake duck'] = menu.find("antiaim",'main', "general", "fakeduck")[2],
    ['Invert desync'] = menu.find("antiaim",'main', "manual", "invert desync")[2],
    ['Invert body lean'] = menu.find("antiaim",'main', "manual", "invert body lean")[2],
    ['Extended angles'] = menu.find("antiaim",'main', "extended angles", "enable")[2],
    ['Freestand'] = menu.find("antiaim",'main', "auto direction", "enable")[2],
    -- misc
    ['Fire extinguisher'] = menu.find('misc','utility','general','fire extinguisher')[2],
    ['Auto-throw'] = menu.find('misc','nade helper','general','autothrow')[2],
}

hVisuals.update_values = function()
    hVisuals.unsorted['Lethal shot'] = menu.find("aimbot", etr.active_weapon(), "target overrides", "lethal shot")[2]
    hVisuals.unsorted['Min. damage'] = menu.find("aimbot", etr.active_weapon(), "target overrides", "min. damage")[2]
    hVisuals.unsorted['Hitbox override'] = menu.find("aimbot", etr.active_weapon(), "target overrides", "hitbox")[2]
    hVisuals.unsorted['Safepoint'] = menu.find("aimbot", etr.active_weapon(), "target overrides", "safepoint")[2]
    hVisuals.unsorted['Lean safepoint'] = menu.find("aimbot", etr.active_weapon(), "target overrides", "body lean safepoint")[2]
    hVisuals.unsorted['Hitchance ovr.'] = menu.find("aimbot", etr.active_weapon(), "target overrides", "hitchance")[2]
    hVisuals.unsorted['Auto-throw'] = menu.find('misc','nade helper','general','autothrow')[2]
end

hVisuals.sorted = { }

local keybinds_drag = etr.new('keybinds', vec2_t(50, script_tbl.screen.y / 2), vec2_t(0, 0))
hVisuals.keybinds = function()
    if not Elements_selection.ref:get('Keybinds') then return end

    keybinds_drag:update()
    hVisuals.update_values()

    local active_binds = { }
    local active_mode = { }
    local pos = keybinds_drag.pos
    local size = keybinds_drag.size
    local offset = 22

    -- inserting active bind
    for name, keys in pairs(hVisuals.unsorted) do
        local mode = keys:get_mode()
        if mode == 3 or mode == 4 then goto skip end

        if keys:get() and not hVisuals.sorted[name] and engine.is_connected() and not menu.is_open() then
            table.insert(hVisuals.sorted, 1, {name = name, mode = hVisuals.mode[mode], alpha = 0})
            hVisuals.sorted[name] = true
        end
        ::skip::
    end

    -- creating alpha and removing bind if not active
    for i = #hVisuals.sorted, 1, -1 do
        local name = hVisuals.sorted[i].name
        local targetAlpha = hVisuals.unsorted[name]:get() and 1 or 0
        hVisuals.sorted[i].alpha = etr.lerp(hVisuals.sorted[i].alpha, targetAlpha, 8)

        if (not hVisuals.unsorted[name]:get()) and (hVisuals.sorted[i].alpha <= 0.15) or menu.is_open() then
            table.remove(hVisuals.sorted, i)
            hVisuals.sorted[name] = false
        end   
    end
    
    -- rendering binds
    for _, tbl in ipairs(hVisuals.sorted) do
        local bind_name = tbl.name
        local bind_size = render.get_text_size(fonts.notifications, bind_name)

        local bind_mode = "[" .. tbl.mode .. "]"
        local mode_size = render.get_text_size(fonts.notifications, bind_mode)

        local alpha = tbl.alpha
        table.insert(active_binds, bind_name)
        table.insert(active_mode, bind_mode)

        render.push_alpha_modifier(alpha)
        render.text(fonts.notifications, bind_name, vec2_t(pos.x, pos.y + offset), color_t(255, 255, 255, 255))
        render.text(fonts.notifications, bind_mode, vec2_t(pos.x + size.x - mode_size.x, pos.y + offset), color_t(255, 255, 255, 255))
        render.pop_alpha_modifier()

        offset = offset + 15 * alpha
    end

    -- rendering other stuff
    local bind_concated = table.concat(active_binds, "\n")
    local mode_concated = table.concat(active_mode, "\n")
    local concated_bind = render.get_text_size(fonts.notifications, bind_concated)
    local concated_mode = render.get_text_size(fonts.notifications, mode_concated)
    local col = elements_col.ref:get()

    keybinds_drag:lerp("size", vec2_t((#active_binds > 0 and (concated_bind.x + concated_mode.x + 35) or 100), 20), 5)
    render.rect_filled(pos, size, color_t(15, 15, 15, 255), 5)
    render.text(fonts.notifications, 'keybinds', vec2_t(pos.x + (size.x / 2), pos.y + (size.y/2)), color_t(255, 255, 255, 255), true)

    render.rect_filled(vec2_t(pos.x, pos.y + (size.y/2) - 5), vec2_t(2, 10), color_t(col.r,col.g,col.b))
    etr.glow(vec2_t(pos.x, pos.y + (size.y/2) - 5), vec2_t(2, 10), 5, color_t(col.r,col.g,col.b, 50), 10)

    render.rect_filled(vec2_t(pos.x + size.x - 2, pos.y + (size.y/2) - 5), vec2_t(2, 10), color_t(col.r,col.g,col.b))
    etr.glow(vec2_t(pos.x + size.x - 2, pos.y + (size.y/2) - 5), vec2_t(2, 10), 5, color_t(col.r,col.g,col.b, 50), 10)
end

local spec_list_drag = etr.new('spectators', vec2_t(230, script_tbl.screen.y / 2), vec2_t(0,0))
local largest_name_size = 0

hVisuals.spectator_list = function()
    if not Elements_selection.ref:get('Spectators') then return end

    local spectators = etr.get_spectators()
    local offset = 20

    local pos = spec_list_drag.pos
    local size = spec_list_drag.size

    if spectators == nil or #spectators <= 0 or menu.is_open() then
        largest_name_size = 100
        goto skip
    end

    for index, value in pairs(spectators) do
        if value == nil or value.name == nil or not value or value.active == false or menu.is_open() then goto skip end
        local name = value.name
        local idx = value.id
        local avatars = etr.get_avatar(name, idx)
        local name_y = render.get_text_size(fonts.notifications, name).y
        local name_size = render.get_text_size(fonts.notifications, name).x

        if name_size > largest_name_size then
            largest_name_size = name_size + name_y + 10
        end

        render.push_alpha_modifier(value.alpha)
        if avatars ~= nil then
            render.texture(avatars.id, vec2_t(pos.x, pos.y + offset), vec2_t(name_y, name_y))
        end
        render.text(fonts.notifications, name, vec2_t(pos.x + name_y + 5, pos.y + offset), color_t(255, 255, 255, 255))
        render.pop_alpha_modifier()
        offset = offset + 15 * value.alpha
        ::skip::
    end
    ::skip::

    spec_list_drag:update()
    spec_list_drag:lerp('size', vec2_t(largest_name_size, 20), 5)
    local col = elements_col.ref:get()

    render.rect_filled(pos, size, color_t(15, 15, 15, 255), 5)
    render.text(fonts.notifications, 'spectators', vec2_t(pos.x + (size.x / 2), pos.y + (size.y/2)), color_t(255, 255, 255, 255), true)

    render.rect_filled(vec2_t(pos.x, pos.y + (size.y/2) - 5), vec2_t(2, 10), color_t(col.r,col.g,col.b))
    etr.glow(vec2_t(pos.x, pos.y + (size.y/2) - 5), vec2_t(2, 10), 5, color_t(col.r,col.g,col.b, 50), 10)

    render.rect_filled(vec2_t(pos.x + size.x - 2, pos.y + (size.y/2) - 5), vec2_t(2, 10), color_t(col.r,col.g,col.b))
    etr.glow(vec2_t(pos.x + size.x - 2, pos.y + (size.y/2) - 5), vec2_t(2, 10), 5, color_t(col.r,col.g,col.b, 50), 10)
end

hMisc.clantag_tbl = {
    '',
    'e',
    'et',
    'etr',
    'etri',
    'etriu',
    'etrium',
    'etrium',
    'etrium',
    'etrium',
    'etriu',
    'etri',
    'etr',
    'et',
    'e',
    ''
}

hMisc.last_change = true
hMisc.last = ""
hMisc.clantag_spammer = function()
    if not clantag_toggle.ref:get() then
        if not hMisc.last_change then client.set_clantag("") end 
        hMisc.last_change = true 
        return 
    end

    if not engine.is_connected() or not engine.is_in_game() then return end

    local tick_rate = engine.get_latency(e_latency_flows.OUTGOING) / globals.interval_per_tick()
    local ticks = globals.tick_count() + tick_rate
    local i = math.floor(math.fmod(ticks / 20, #hMisc.clantag_tbl)) + 1

    hMisc.last_change = false 
    if hMisc.last == hMisc.clantag_tbl[i] then return end
    client.set_clantag(hMisc.clantag_tbl[i])
    hMisc.last = hMisc.clantag_tbl[i]
end

hMisc.phrases = {
    [1] = {
        '1 botik',
        'owned puppy',
        'by hvh bo$$',
        'owned you like a nigger',
        '[DEAD]',
        'cy@ yt',
        'who.ru',
        'quit hvh',
        'iq < 0',
        're$olved',
        'nice iq botik',
        'why would u do that bot',
        'so ez',
        'dog nn', 
        'nice antiaim you got bot',
        'shit on'
    },
    [2] = {
        'etrium.dev, best lua',
        'search etrium.dev on browser, to start owning',
        'etrium.dev AA power (◣_◢)',
        'stop wasting money on shit luas, etrium.dev is all you need!'
    }
}

hMisc.old_phrase = ""
hMisc.killsay = function(event) 
    if event.name ~= 'player_death' or not killsay_toggle.ref:get() then return end

    local lp = entity_list.get_local_player()
    if not lp or lp == nil then return end

    local attacker = entity_list.get_player_from_userid(event.attacker)
    if not attacker or attacker == nil then return end

    local kill_type = killsay_type.ref:get()

    ::generate::

    local txt = hMisc.phrases[kill_type][math.random(1, #hMisc.phrases[kill_type])]

    if txt == hMisc.old_phrase then goto generate end

    if attacker == lp then
        client.delay_call(function()
            engine.execute_cmd('say ' .. txt)
        end, killsay_delay.ref:get())
    end
    hMisc.old_phrase = txt
end

-- [[ Configs ]] --
hConfigs.export = function()
    local data = ui.export()
    data = base64.encode(data)

    etr.clipboard_export(data)
end

hConfigs.import = function()
    local data = etr.clipboard_import()
    data = base64.decode(data)
    
    local success, result = pcall(ui.import, data)
    if not success then print(result) else print("Loaded config!") end
end

config_col_s:button('Export', hConfigs.export)
config_col_s:button('Import', hConfigs.import)

hConfigs.list = { }
hConfigs.dir = "etrium/"
hConfigs.default_dir = "etrium/default.etr"
hConfigs.default_config = "eyJSYWdlIjpbWyJXaGVuIGFjdGl2YXRlIiwwLjVdLFsiQmFpbSBpbiBhaXIgZW5lbWllcyIsdHJ1ZV1dLCJEZXN5bmMgW1J1bl0iOltbIk9uIHNob3QiLDJdLFsiTGVmdCIsMTAwXSxbIlJpZ2h0IiwxMDBdLFsiRGVzeW5jIHNpZGUiLDRdXSwiRGVmZW5zaXZlIFtSdW5dIjpbWyJFbmFibGUiLHRydWVdLFsiVHJpZ2dlciIsMl1dLCJEZXN5bmMgW1N0YW5kXSI6W1siT24gc2hvdCIsMl0sWyJMZWZ0IiwxMDBdLFsiUmlnaHQiLDEwMF0sWyJEZXN5bmMgc2lkZSIsM11dLCJBbmdsZXMgW1Nsb3cgd2Fsa10iOltbIkppdHRlciBhZGQiLDI3XSxbIkRlbGF5IGppdHRlciIsM10sWyJZYXcgYWRkIiwtNl0sWyJKaXR0ZXIgdHlwZSIsMl0sWyJQaXRjaCIsMl0sWyJDdXN0b20gcGl0Y2giLDBdLFsiWWF3IiwzXSxbIkppdHRlciBtb2RlIiwyXV0sIlNvY2lhbHMiOltdLCJEZWZlbnNpdmUgW1N0YW5kXSI6W1siRW5hYmxlIixmYWxzZV0sWyJUcmlnZ2VyIiwxXV0sIkRlc3luYyBbQWlyXSI6W1siT24gc2hvdCIsNF0sWyJMZWZ0IiwxMDBdLFsiUmlnaHQiLDEwMF0sWyJEZXN5bmMgc2lkZSIsNF1dLCJIb21lIjpbWyJIaWRlIGNoYW5nZWxvZ3MiLHRydWVdXSwiRmFrZWxhZyBbQWlyIGR1Y2tdIjpbWyJCcmVhayBMQyIsZmFsc2VdLFsiRmFrZWxhZyIsMF1dLCJNYWluIjpbWyJPdmVycmlkZSBDcm91Y2ggY29uZGl0aW9uIix0cnVlXSxbIk92ZXJyaWRlIEFpciBkdWNrIGNvbmRpdGlvbiIsZmFsc2VdLFsiT3ZlcnJpZGUgQWlyIGNvbmRpdGlvbiIsdHJ1ZV0sWyJCYXNlIHNldHRpbmdzIiwxXSxbIk92ZXJyaWRlIFNsb3cgd2FsayBjb25kaXRpb24iLHRydWVdLFsiT3ZlcnJpZGUgVXNlIGNvbmRpdGlvbiIsZmFsc2VdLFsiT3ZlcnJpZGUgUnVuIGNvbmRpdGlvbiIsdHJ1ZV0sWyJPdmVycmlkZSBTdGFuZCBjb25kaXRpb24iLHRydWVdXSwiQW5nbGVzIFtDcm91Y2hdIjpbWyJKaXR0ZXIgYWRkIiwtMTldLFsiRGVsYXkgaml0dGVyIiw0XSxbIllhdyBhZGQiLDRdLFsiSml0dGVyIHR5cGUiLDVdLFsiUGl0Y2giLDJdLFsiQ3VzdG9tIHBpdGNoIiwwXSxbIllhdyIsM10sWyJKaXR0ZXIgbW9kZSIsMl1dLCJGYWtlbGFnIFtSdW5dIjpbWyJCcmVhayBMQyIsdHJ1ZV0sWyJGYWtlbGFnIiwxNV1dLCJPcHRpb25zIFtDcm91Y2hdIjpbWyJZYXcgb2Zmc2V0IiwyMl0sWyJTcGluIixmYWxzZV0sWyJQaXRjaCIsNV0sWyJDdXN0b20gcGl0Y2giLDBdLFsiQW50aS1wcmVkaXRjdCIsdHJ1ZV1dLCJEZXN5bmMgW1VzZV0iOltbIk9uIHNob3QiLDFdLFsiTGVmdCIsMF0sWyJSaWdodCIsMF0sWyJEZXN5bmMgc2lkZSIsMV1dLCJPcHRpb25zIFtTbG93IHdhbGtdIjpbWyJZYXcgb2Zmc2V0IiwwXSxbIlNwaW4iLGZhbHNlXSxbIlBpdGNoIiwxXSxbIkN1c3RvbSBwaXRjaCIsMF0sWyJBbnRpLXByZWRpdGN0IixmYWxzZV1dLCJBbmdsZXMgW1VzZV0iOltbIkppdHRlciBhZGQiLDBdLFsiRGVsYXkgaml0dGVyIiwwXSxbIllhdyBhZGQiLDBdLFsiSml0dGVyIHR5cGUiLDFdLFsiUGl0Y2giLDFdLFsiQ3VzdG9tIHBpdGNoIiwwXSxbIllhdyIsMV0sWyJKaXR0ZXIgbW9kZSIsMV1dLCJGYWtlbGFnIFtTdGFuZF0iOltbIkJyZWFrIExDIixmYWxzZV0sWyJGYWtlbGFnIiwxXV0sIkRlZmVuc2l2ZSBbQWlyXSI6W1siRW5hYmxlIix0cnVlXSxbIlRyaWdnZXIiLDJdXSwiRGVzeW5jIFtDcm91Y2hdIjpbWyJPbiBzaG90IiwyXSxbIkxlZnQiLDEwMF0sWyJSaWdodCIsMTAwXSxbIkRlc3luYyBzaWRlIiwyXV0sIkZha2VsYWcgW0Fpcl0iOltbIkJyZWFrIExDIix0cnVlXSxbIkZha2VsYWciLDE1XV0sIkZha2VsYWcgW1Nsb3cgd2Fsa10iOltbIkJyZWFrIExDIix0cnVlXSxbIkZha2VsYWciLDNdXSwiQW50aS1CcnV0ZWZvcmNlIjpbWyJFbmFibGUiLHRydWVdLFsiUmVzZXQgdGltZXIiLDIwXV0sIkFuZ2xlcyBbUnVuXSI6W1siSml0dGVyIGFkZCIsMTldLFsiRGVsYXkgaml0dGVyIiwzXSxbIllhdyBhZGQiLC00XSxbIkppdHRlciB0eXBlIiw1XSxbIlBpdGNoIiwyXSxbIkN1c3RvbSBwaXRjaCIsMF0sWyJZYXciLDNdLFsiSml0dGVyIG1vZGUiLDJdXSwiRGVmZW5zaXZlIFtDcm91Y2hdIjpbWyJFbmFibGUiLHRydWVdLFsiVHJpZ2dlciIsMV1dLCJNaXNjIjpbWyJDbGFudGFnIix0cnVlXSxbIktpbGwgc2F5Iix0cnVlXSxbIkRlbGF5Iiw1XSxbIlR5cGUiLDFdXSwiU3VidGFicyI6W1siQ29uZGl0aW9uIiw0XV0sIkFuZ2xlcyBbQWlyXSI6W1siSml0dGVyIGFkZCIsMzZdLFsiRGVsYXkgaml0dGVyIiwwXSxbIllhdyBhZGQiLDRdLFsiSml0dGVyIHR5cGUiLDRdLFsiUGl0Y2giLDJdLFsiQ3VzdG9tIHBpdGNoIiwwXSxbIllhdyIsM10sWyJKaXR0ZXIgbW9kZSIsM11dLCJGYWtlbGFnIFtDcm91Y2hdIjpbWyJCcmVhayBMQyIsdHJ1ZV0sWyJGYWtlbGFnIiwxNV1dLCJEZWZlbnNpdmUgW1VzZV0iOltbIkVuYWJsZSIsZmFsc2VdLFsiVHJpZ2dlciIsMV1dLCJEZXN5bmMgW0FpciBkdWNrXSI6W1siT24gc2hvdCIsMV0sWyJMZWZ0IiwwXSxbIlJpZ2h0IiwwXSxbIkRlc3luYyBzaWRlIiwxXV0sIk9wdGlvbnMgW1N0YW5kXSI6W1siWWF3IG9mZnNldCIsMF0sWyJTcGluIixmYWxzZV0sWyJQaXRjaCIsMV0sWyJDdXN0b20gcGl0Y2giLDBdLFsiQW50aS1wcmVkaXRjdCIsdHJ1ZV1dLCJPcHRpb25zIFtBaXIgZHVja10iOltbIllhdyBvZmZzZXQiLDBdLFsiU3BpbiIsZmFsc2VdLFsiUGl0Y2giLDFdLFsiQ3VzdG9tIHBpdGNoIiwwXSxbIkFudGktcHJlZGl0Y3QiLGZhbHNlXV0sIk9wdGlvbnMgW1VzZV0iOltbIllhdyBvZmZzZXQiLDBdLFsiU3BpbiIsZmFsc2VdLFsiUGl0Y2giLDFdLFsiQ3VzdG9tIHBpdGNoIiwwXSxbIkFudGktcHJlZGl0Y3QiLGZhbHNlXV0sIkZha2VsYWcgW1VzZV0iOltbIkJyZWFrIExDIixmYWxzZV0sWyJGYWtlbGFnIiwwXV0sIkluZGljYXRvcnMiOltbIkFuaW0uIHNwZWVkIiwzXSxbImJhciBjb2xvciIsMjU1LDI1NSwyNTUsMjU1XSxbImRlZmVuc2l2ZSBjb2xvciIsMjU1LDI1NSwyNTUsMjU1XSxbIk9wdGlvbnMiLFtbIkFuaW1hdGlvbiBzcGVlZCIsdHJ1ZV0sWyJTbG93ZWQiLHRydWVdLFsiU2NvcGVkIGFuaW0uIixmYWxzZV1dXSxbIlRpbWUiLDddLFsiRGVmZW5zaXZlIGluZGljYXRvciIsdHJ1ZV0sWyJwcmVmaXggY29sb3IiLDI1NSwyNTUsMjU1LDI1NV0sWyJOb3RpZmljYXRpb25zIixbWyJIaXQiLHRydWVdLFsiTWlzcyIsdHJ1ZV0sWyJBbnRpYnJ1dGUiLHRydWVdXV0sWyJDcm9zc2hhaXIgaW5kaWNhdG9ycyIsdHJ1ZV0sWyJjcm9zcyBjb2xvciIsMjU1LDI1NSwyNTUsMjU1XV0sIkRlZmVuc2l2ZSBbQWlyIGR1Y2tdIjpbWyJFbmFibGUiLGZhbHNlXSxbIlRyaWdnZXIiLDFdXSwiT3B0aW9ucyI6W1siQW5pbS4gYnJlYWtlcnMiLFtbIjAgcGl0Y2ggb24gbGFuZCIsZmFsc2VdLFsiU3RhdGljIGxlZ3MgaW4gYWlyIixmYWxzZV0sWyJXYWxrIGluIGFpciIsdHJ1ZV0sWyJMZWFuIGluIGFpciIsZmFsc2VdXV1dLCJFbGVtZW50cyI6W1sidWkgY29sb3IiLDIzOSwyMzksMjM5LDI1NV0sWyJDaGFuZ2UgdGltZSBmb3JtYXQiLHRydWVdLFsiVUkgZWxlbWVudHMiLFtbIldhdGVybWFyayIsdHJ1ZV0sWyJTcGVjdGF0b3JzIixmYWxzZV0sWyJLZXliaW5kcyIsdHJ1ZV1dXSxbIldhdGVybWFyayBzZWxlY3Rpb24iLFtbIk5hbWUiLHRydWVdLFsiUGluZyIsdHJ1ZV0sWyJGcHMiLHRydWVdLFsiVGltZSIsdHJ1ZV1dXV0sIkFuZ2xlcyBbU3RhbmRdIjpbWyJKaXR0ZXIgYWRkIiwxMV0sWyJEZWxheSBqaXR0ZXIiLDldLFsiWWF3IGFkZCIsNF0sWyJKaXR0ZXIgdHlwZSIsNV0sWyJQaXRjaCIsMl0sWyJDdXN0b20gcGl0Y2giLDBdLFsiWWF3IiwzXSxbIkppdHRlciBtb2RlIiwyXV0sIkFuZ2xlcyBbQWlyIGR1Y2tdIjpbWyJKaXR0ZXIgYWRkIiwwXSxbIkRlbGF5IGppdHRlciIsMF0sWyJZYXcgYWRkIiwwXSxbIkppdHRlciB0eXBlIiwxXSxbIlBpdGNoIiwxXSxbIkN1c3RvbSBwaXRjaCIsMF0sWyJZYXciLDFdLFsiSml0dGVyIG1vZGUiLDFdXSwiQ29uZmlncyBbdGVtcF0iOltdLCJFdHJpdW0uY29kZXMgW2JldGFdIjpbWyJDaG9vc2UgdGFiIiwxXV0sIk9wdGlvbnMgW1J1bl0iOltbIllhdyBvZmZzZXQiLDgyXSxbIlNwaW4iLHRydWVdLFsiUGl0Y2giLDZdLFsiQ3VzdG9tIHBpdGNoIiwtNjVdLFsiQW50aS1wcmVkaXRjdCIsdHJ1ZV1dLCJEZWZlbnNpdmUgW1Nsb3cgd2Fsa10iOltbIkVuYWJsZSIsZmFsc2VdLFsiVHJpZ2dlciIsMV1dLCJPcHRpb25zIFtBaXJdIjpbWyJZYXcgb2Zmc2V0Iiw0MV0sWyJTcGluIix0cnVlXSxbIlBpdGNoIiw2XSxbIkN1c3RvbSBwaXRjaCIsLTYyXSxbIkFudGktcHJlZGl0Y3QiLHRydWVdXSwiRGVzeW5jIFtTbG93IHdhbGtdIjpbWyJPbiBzaG90IiwyXSxbIkxlZnQiLDEwMF0sWyJSaWdodCIsMTAwXSxbIkRlc3luYyBzaWRlIiw0XV19"
local cloud_list = config_col:list('Configs', hConfigs.list, 4)
local name_config = config_col:text_input('Config name')
local save_config = config_col:button('Save', function() hConfigs.save_config() end)
local load_config = config_col:button('Load', function() hConfigs.load_config() end)
local create_config = config_col:button('Create', function() hConfigs.create_config() end) 
local refresh_config = config_col:button('Refresh', function() hConfigs.reload_config() end)
local delete_config = config_col:button('Delete', function() hConfigs.delete_config() end)
 
hConfigs.reload_config = function()
    local files = etr.get_files()
    cloud_list.ref:set_items(files)
end

hConfigs.setup_configs = function()
    local exits = etr.is_directory(hConfigs.dir)
    if not exits then
        etr.create_directory(hConfigs.dir, "")
    end

    etr.write(hConfigs.default_dir, hConfigs.default_config)
    hConfigs.reload_config()
end
hConfigs.setup_configs()

hConfigs.load_config = function()
    local cur = cloud_list.ref:get_active_item_name()
    local dir = hConfigs.dir .. "" .. cur
    
    local exists = etr.exists(dir)
    if not exists then 
        hVisuals.notify("Invalid file path")
        return
    end

    local config = etr.read(dir)
    config = base64.decode(config)
    local success, err = pcall(ui.import, config)

    if success then
        hVisuals.notify("Config successfully loaded")
    else
        hVisuals.notify("Config failed to load " .. err)
    end
end

hConfigs.save_config = function()
    local cur = cloud_list.ref:get_active_item_name()
    if cur == "default.etr" then 
        hVisuals.notify("Can't save to default config")
        return
    end

    local dir = hConfigs.dir .. "" .. cur
    local exists = etr.exists(dir)
    if not exists then
        hVisuals.notify("Config file doesn't exist")
        return
    end

    local config = ui.export()
    config = base64.encode(config)
    etr.write(dir, config)
    hVisuals.notify("Successfully saved config")
end

hConfigs.create_config = function()
    local name = name_config.ref:get()
    local dir = hConfigs.dir .. "" .. name .. ".etr"

    local len = string.len(name)
    if len <= 0 then
        hVisuals.notify("Invalid file name")
        return
    end

    local exists = etr.exists(dir)
    if exists then
        hVisuals.notify("Config already exists")
        return
    end

    etr.write(dir, "")
    cloud_list.ref:add_item(name .. ".etr")
    hVisuals.notify("Successfully created config")
end

hConfigs.delete_config = function()
    local active = cloud_list.ref:get_active_item_name()
    local dir = hConfigs.dir .. "" .. active

    if active == "default.etr" then 
        hVisuals.notify("You can't delete default config")
        return
    end

    local exists = etr.exists(dir)
    if not exists then
        hVisuals.notify("Config file doesn't exist")
        return
    end

    etr.delete(dir, "game")
    cloud_list.ref:remove_item(active)
    hVisuals.notify("Successfully deleted config")
end


-- [[Callbacks]] --
hCallbacks.on_paint = function()
    visibility_func()
    sesion_time_Updater()
    hVisuals.defensive_ind()
    hVisuals.crosshair_indicator()
    hVisuals.notifications()
    hVisuals.changelogs()
    hVisuals.watermark()
    hVisuals.keybinds()
    hVisuals.spectator_list()
    hMisc.clantag_spammer()
end

hCallbacks.on_hit = function(hit)
    hVisuals.hitlogs(hit)
end

hCallbacks.on_miss = function(miss)
    hVisuals.misslogs(miss)
end

hCallbacks.on_shoot = function()

end

hCallbacks.on_antiaim = function(ctx, cmd, unpred)
    hAntiaim.handle(ctx, cmd, unpred)
    hAntiaim.abrute_pred(ctx)
    hAntiaim.breakers(ctx)
end

hCallbacks.hit_scan = function(ctx)
    hRage.baim_in_air_enm(ctx)
end

hCallbacks.events = function(data)
    hAntiaim.bruteforce(data)
    hMisc.killsay(data)
end

callbacks.add(e_callbacks.PAINT, hCallbacks.on_paint)
callbacks.add(e_callbacks.ANTIAIM, hCallbacks.on_antiaim)
callbacks.add(e_callbacks.AIMBOT_HIT, hCallbacks.on_hit)
callbacks.add(e_callbacks.AIMBOT_MISS, hCallbacks.on_miss)
callbacks.add(e_callbacks.HITSCAN, hCallbacks.hit_scan)
callbacks.add(e_callbacks.EVENT, hCallbacks.events)
callbacks.add(e_callbacks.SHUTDOWN, function() client.set_clantag('') end)