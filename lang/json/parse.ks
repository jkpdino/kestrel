public enum Json {
  case String(String)
  case Number(Float)
  case Boolean(Bool)

  case Array([Json])
  case Object([String: Json])
}

// 0-9 => Number
// " => String
// t / f => Bool
// [ => Array
// { => Object