// Realistic type alias usage scenarios
module Application.Types

// Common type aliases for clarity
public type UserID = String;
public type Email = String;
public type PhoneNumber = String;
public type Timestamp = Int;

// Domain-specific aliases
type CustomerID = UserID;
type OrderID = String;
type ProductID = String;

// Collection aliases
public type UserList = Array;
public type UserMap = Dictionary;
type UserSet = Set;

// Result type aliases
public type Result = Either;
public type Maybe = Optional;
type Error = ErrorType;

// Callback type aliases (if function types are supported)
type Callback = Function;
type Handler = EventHandler;
type Validator = ValidationFunction;

// Complex domain model
private type AccountBalance = Decimal;
private type TransactionAmount = Decimal;
internal type Currency = CurrencyCode;

// API-related types
public type Response = HTTPResponse;
public type Request = HTTPRequest;
type Headers = HeaderMap;
type QueryParams = ParameterMap;

// Configuration types
fileprivate type Config = Configuration;
fileprivate type Settings = ApplicationSettings;
internal type Environment = EnvironmentType;

// Data transfer objects
public type UserDTO = UserDataTransferObject;
public type OrderDTO = OrderDataTransferObject;
type ProductDTO = ProductDataTransferObject;

// Generic wrapper aliases (using identifiers)
type Wrapped = Wrapper;
type Boxed = Box;
type Referenced = Ref;

// State management
type State = ApplicationState;
type Action = StateAction;
type Reducer = StateReducer;

// Visibility showcase in realistic context
public type PublicAPI = ExternalInterface;
internal type InternalAPI = InternalInterface;
private type PrivateImplementation = Implementation;
fileprivate type FileUtils = UtilityFunctions;
