public enum Json {
  case String(String)
  case Number(Float)
  case Boolean(Bool)

  case Array([Json])
  case Object([String: Json])
}

public enum JsonError {
  case unexpectedEof
  case invalidBoolean
  case invalidNumber
  case invalidEscape
  case unexpectedChar(Char)
}

// 0-9 => Number
// " => String
// t / f => Bool
// [ => Array
// { => Object

func parse(json: String) -> Json throws JsonError {
  var jsonCharacters = json.characters() -> Peekable;

  let json = parse(jsonCharacters);

  if let nextChar = jsonCharacters.next() {
    throw .unexpectedChar(nextChar)
  }

  return json;
}

private func parse(head: ref Peekable[CharacterIterator]) -> Json throws JsonError {
  let nextCharacter = jsonCharacters.peek()

  match nextCharacter {
    "0".."9" => parseNumber(head),
    "\"" => parseString(head),
    "t", "f" => parseBoolean(head),
    "[" => parseArray(head),
    "{" => parseObject(head)
  }
}

private func parseString(head: ref Peekable[CharacterIterator]) -> Json throws JsonError {
  // consume opening quote
  head.next()

  var result = ""

  while let char = head.peek() {
    if char == "\"" {
      head.next()
      return Json.String(result)
    } else if char == "\\" {
      head.next()
      // handle escape sequences
      match head.peek() {
        "\"" => result += "\""
        "\\" => result += "\\"
        "/" => result += "/"
        "b" => result += "\b"
        "f" => result += "\f"
        "n" => result += "\n"
        "r" => result += "\r"
        "t" => result += "\t"
      }
      head.next()
    } else {
      result += char
      head.next()
    }
  }

  throw JsonError.unexpectedEof
}

private func parseBoolean(head: ref Peekable[CharacterIterator]) -> Json throws JsonError {
  if head.peek() == "t" {
    head.next()
    if head.peek() == "r" { head.next() }
    if head.peek() == "u" { head.next() }
    if head.peek() == "e" { head.next() }
    return Json.Boolean(true)
  } else if head.peek() == "f" {
    head.next()
    if head.peek() == "a" { head.next() }
    if head.peek() == "l" { head.next() }
    if head.peek() == "s" { head.next() }
    if head.peek() == "e" { head.next() }
    return Json.Boolean(false)
  }

  throw JsonError.invalidBoolean
}

private func parseArray(head: ref Peekable[CharacterIterator]) -> Json throws JsonError {
  // consume opening bracket
  head.next()

  var array: [Json] = []

  while let char = head.peek() {
    if char == "]" {
      head.next()
      return Json.Array(array)
    }

    if char == "," {
      head.next()
    } else {
      array.append(parse(head))
    }
  }

  throw JsonError.unexpectedEof
}

private func parseObject(head: ref Peekable[CharacterIterator]) -> Json throws JsonError {
  // consume opening brace
  head.next()

  var object: [String: Json] = [:]

  while let char = head.peek() {
    if char == "}" {
      head.next()
      return Json.Object(object)
    }

    if char == "," {
      head.next()
    } else if char == "\"" {
      // parse key
      let keyJson = parseString(head)

      match keyJson {
        case .String(let key) => {
          // consume colon
          head.next()

          // parse value
          let value = parse(head)
          object[key] = value
        }
      }
    }
  }

  throw JsonError.unexpectedEof
}

private func parseNumber(head: ref Peekable[CharacterIterator]) -> Json throws JsonError {
  var number = ""

  // optional minus sign
  if head.peek() == "-" {
    number += "-"
    head.next()
  }

  // integer part
  if head.peek() == "0" {
    number += "0"
    head.next()
  } else if let char = head.peek(), char >= "1" && char <= "9" {
    while let char = head.peek(), char >= "0" && char <= "9" {
      number += char
      head.next()
    }
  } else {
    throw JsonError.invalidNumber
  }

  // decimal part (optional)
  if head.peek() == "." {
    number += "."
    head.next()

    if let char = head.peek(), char >= "0" && char <= "9" {
      while let char = head.peek(), char >= "0" && char <= "9" {
        number += char
        head.next()
      }
    } else {
      throw JsonError.invalidNumber
    }
  }

  // exponent part (optional)
  if head.peek() == "e" || head.peek() == "E" {
    number += head.peek()!
    head.next()

    if head.peek() == "+" || head.peek() == "-" {
      number += head.peek()!
      head.next()
    }

    if let char = head.peek(), char >= "0" && char <= "9" {
      while let char = head.peek(), char >= "0" && char <= "9" {
        number += char
        head.next()
      }
    } else {
      throw JsonError.invalidNumber
    }
  }

  // convert to float
  match Float.parse(number) {
    case .some(let value) => return Json.Number(value)
    case .none => throw JsonError.invalidNumber
  }
}