module StateMachine

// ============================================================================
// A Generic State Machine Framework
// ============================================================================
// Demonstrates: advanced generics, protocol constraints, type parameters,
// function types, nested generics, and real-world design patterns

// --- Core State Machine Protocols ---

protocol State {
    func getName() -> String
    func canTransitionTo(next: Self) -> Bool
}

protocol Event {
    func getType() -> String
    func getTimestamp() -> Int
}

protocol Transition[S, E] where S: State, E: Event {
    func getFrom() -> S
    func getTo() -> S
    func getTrigger() -> E
    func isValid() -> Bool
}

protocol StateObserver[S] where S: State {
    func onStateEntered(state: S)
    func onStateExited(state: S)
}

// --- Generic State Machine ---

struct StateMachine[S, E] where S: State, E: Event {
    var currentState: S
    var history: [StateTransitionRecord[S, E]]
    var observers: [StateObserver[S]]
    var isRunning: Bool

    init(initialState: S) {
        self.currentState = initialState
        self.history = []
        self.observers = []
        self.isRunning = false
    }

    func getCurrentState() -> S { self.currentState }
    func getHistory() -> [StateTransitionRecord[S, E]] { self.history }
    func isInState(state: S) -> Bool { }
    
    mutating func start() { self.isRunning = true }
    mutating func stop() { self.isRunning = false }
    mutating func reset(to state: S) { self.currentState = state }
    
    mutating func dispatch(event: E) -> Bool { }
    mutating func addObserver(observer: StateObserver[S]) { }
}

struct StateTransitionRecord[S, E] where S: State, E: Event {
    let fromState: S
    let toState: S
    let event: E
    let timestamp: Int
    let success: Bool

    init(from: S, to: S, triggeredBy event: E, at timestamp: Int) {
        self.fromState = from
        self.toState = to
        self.event = event
        self.timestamp = timestamp
        self.success = true
    }

    func getFromState() -> S { self.fromState }
    func getToState() -> S { self.toState }
    func getEvent() -> E { self.event }
}

// --- HTTP Request State Machine Example ---

struct HttpRequestState: State {
    let name: String
    let isTerminal: Bool

    init(name: String, terminal: Bool) {
        self.name = name
        self.isTerminal = terminal
    }

    func getName() -> String { self.name }
    func canTransitionTo(next: HttpRequestState) -> Bool { }
    func isTerminalState() -> Bool { self.isTerminal }

    static func idle() -> HttpRequestState { HttpRequestState(name: "idle", terminal: false) }
    static func connecting() -> HttpRequestState { HttpRequestState(name: "connecting", terminal: false) }
    static func sending() -> HttpRequestState { HttpRequestState(name: "sending", terminal: false) }
    static func receiving() -> HttpRequestState { HttpRequestState(name: "receiving", terminal: false) }
    static func success() -> HttpRequestState { HttpRequestState(name: "success", terminal: true) }
    static func failed() -> HttpRequestState { HttpRequestState(name: "failed", terminal: true) }
    static func cancelled() -> HttpRequestState { HttpRequestState(name: "cancelled", terminal: true) }
}

struct HttpEvent: Event {
    let eventType: String
    let timestamp: Int
    let payload: String

    init(eventType: String, at timestamp: Int) {
        self.eventType = eventType
        self.timestamp = timestamp
        self.payload = ""
    }

    init(eventType: String, at timestamp: Int, withPayload payload: String) {
        self.eventType = eventType
        self.timestamp = timestamp
        self.payload = payload
    }

    func getType() -> String { self.eventType }
    func getTimestamp() -> Int { self.timestamp }
    func getPayload() -> String { self.payload }

    static func connect() -> HttpEvent { HttpEvent(eventType: "connect", at: 0) }
    static func send() -> HttpEvent { HttpEvent(eventType: "send", at: 0) }
    static func receive() -> HttpEvent { HttpEvent(eventType: "receive", at: 0) }
    static func complete() -> HttpEvent { HttpEvent(eventType: "complete", at: 0) }
    static func error(message: String) -> HttpEvent { HttpEvent(eventType: "error", at: 0, withPayload: message) }
    static func cancel() -> HttpEvent { HttpEvent(eventType: "cancel", at: 0) }
}

type HttpStateMachine = StateMachine[HttpRequestState, HttpEvent];

// --- Order Processing State Machine Example ---

struct OrderState: State {
    let name: String
    let allowsModification: Bool
    let allowsCancellation: Bool

    init(name: String, canModify: Bool, canCancel: Bool) {
        self.name = name
        self.allowsModification = canModify
        self.allowsCancellation = canCancel
    }

    func getName() -> String { self.name }
    func canTransitionTo(next: OrderState) -> Bool { }
    func canModify() -> Bool { self.allowsModification }
    func canCancel() -> Bool { self.allowsCancellation }

    static func draft() -> OrderState { OrderState(name: "draft", canModify: true, canCancel: true) }
    static func submitted() -> OrderState { OrderState(name: "submitted", canModify: false, canCancel: true) }
    static func processing() -> OrderState { OrderState(name: "processing", canModify: false, canCancel: true) }
    static func shipped() -> OrderState { OrderState(name: "shipped", canModify: false, canCancel: false) }
    static func delivered() -> OrderState { OrderState(name: "delivered", canModify: false, canCancel: false) }
    static func cancelled() -> OrderState { OrderState(name: "cancelled", canModify: false, canCancel: false) }
    static func refunded() -> OrderState { OrderState(name: "refunded", canModify: false, canCancel: false) }
}

struct OrderEvent: Event {
    let eventType: String
    let timestamp: Int
    let orderId: String
    let metadata: String

    init(eventType: String, forOrder orderId: String, at timestamp: Int) {
        self.eventType = eventType
        self.orderId = orderId
        self.timestamp = timestamp
        self.metadata = ""
    }

    func getType() -> String { self.eventType }
    func getTimestamp() -> Int { self.timestamp }
    func getOrderId() -> String { self.orderId }

    static func submit(orderId: String) -> OrderEvent { OrderEvent(eventType: "submit", forOrder: orderId, at: 0) }
    static func process(orderId: String) -> OrderEvent { OrderEvent(eventType: "process", forOrder: orderId, at: 0) }
    static func ship(orderId: String) -> OrderEvent { OrderEvent(eventType: "ship", forOrder: orderId, at: 0) }
    static func deliver(orderId: String) -> OrderEvent { OrderEvent(eventType: "deliver", forOrder: orderId, at: 0) }
    static func cancel(orderId: String) -> OrderEvent { OrderEvent(eventType: "cancel", forOrder: orderId, at: 0) }
    static func refund(orderId: String) -> OrderEvent { OrderEvent(eventType: "refund", forOrder: orderId, at: 0) }
}

type OrderStateMachine = StateMachine[OrderState, OrderEvent];

// --- Authentication State Machine Example ---

struct AuthState: State {
    let name: String
    let isAuthenticated: Bool
    let requiresMFA: Bool

    init(name: String, authenticated: Bool, mfaRequired: Bool) {
        self.name = name
        self.isAuthenticated = authenticated
        self.requiresMFA = mfaRequired
    }

    func getName() -> String { self.name }
    func canTransitionTo(next: AuthState) -> Bool { }
    func isLoggedIn() -> Bool { self.isAuthenticated }
    func needsMFA() -> Bool { self.requiresMFA }

    static func anonymous() -> AuthState { AuthState(name: "anonymous", authenticated: false, mfaRequired: false) }
    static func authenticating() -> AuthState { AuthState(name: "authenticating", authenticated: false, mfaRequired: false) }
    static func awaitingMFA() -> AuthState { AuthState(name: "awaiting_mfa", authenticated: false, mfaRequired: true) }
    static func authenticated() -> AuthState { AuthState(name: "authenticated", authenticated: true, mfaRequired: false) }
    static func sessionExpired() -> AuthState { AuthState(name: "session_expired", authenticated: false, mfaRequired: false) }
    static func locked() -> AuthState { AuthState(name: "locked", authenticated: false, mfaRequired: false) }
}

struct AuthEvent: Event {
    let eventType: String
    let timestamp: Int
    let userId: String
    let sessionId: String

    init(eventType: String, at timestamp: Int) {
        self.eventType = eventType
        self.timestamp = timestamp
        self.userId = ""
        self.sessionId = ""
    }

    init(eventType: String, forUser userId: String, withSession sessionId: String, at timestamp: Int) {
        self.eventType = eventType
        self.userId = userId
        self.sessionId = sessionId
        self.timestamp = timestamp
    }

    func getType() -> String { self.eventType }
    func getTimestamp() -> Int { self.timestamp }
    func getUserId() -> String { self.userId }
    func getSessionId() -> String { self.sessionId }

    static func login(userId: String) -> AuthEvent { AuthEvent(eventType: "login", forUser: userId, withSession: "", at: 0) }
    static func mfaChallenge() -> AuthEvent { AuthEvent(eventType: "mfa_challenge", at: 0) }
    static func mfaVerified() -> AuthEvent { AuthEvent(eventType: "mfa_verified", at: 0) }
    static func logout() -> AuthEvent { AuthEvent(eventType: "logout", at: 0) }
    static func sessionTimeout() -> AuthEvent { AuthEvent(eventType: "session_timeout", at: 0) }
    static func lockAccount() -> AuthEvent { AuthEvent(eventType: "lock_account", at: 0) }
    static func unlockAccount() -> AuthEvent { AuthEvent(eventType: "unlock_account", at: 0) }
}

type AuthStateMachine = StateMachine[AuthState, AuthEvent];

// --- State Machine Builder ---

struct TransitionRule[S, E] where S: State, E: Event {
    let fromState: S
    let toState: S
    let eventType: String
    let guard: String

    init(from: S, to: S, on eventType: String) {
        self.fromState = from
        self.toState = to
        self.eventType = eventType
        self.guard = ""
    }

    init(from: S, to: S, on eventType: String, when guard: String) {
        self.fromState = from
        self.toState = to
        self.eventType = eventType
        self.guard = guard
    }

    func matches(event: E, currentState: S) -> Bool { }
}

struct StateMachineBuilder[S, E] where S: State, E: Event {
    var initialState: S
    var rules: [TransitionRule[S, E]]
    var onEnterActions: [(S, String)]
    var onExitActions: [(S, String)]

    init(startingWith state: S) {
        self.initialState = state
        self.rules = []
        self.onEnterActions = []
        self.onExitActions = []
    }

    func addTransition(from: S, to: S, on eventType: String) -> StateMachineBuilder[S, E] { }
    func addTransition(from: S, to: S, on eventType: String, when guard: String) -> StateMachineBuilder[S, E] { }
    func onEnter(state: S, action: String) -> StateMachineBuilder[S, E] { }
    func onExit(state: S, action: String) -> StateMachineBuilder[S, E] { }
    func build() -> StateMachine[S, E] { StateMachine(initialState: self.initialState) }
}

// --- Utility Types ---

type HttpTransitionRule = TransitionRule[HttpRequestState, HttpEvent];
type OrderTransitionRule = TransitionRule[OrderState, OrderEvent];
type AuthTransitionRule = TransitionRule[AuthState, AuthEvent];

type HttpMachineBuilder = StateMachineBuilder[HttpRequestState, HttpEvent];
type OrderMachineBuilder = StateMachineBuilder[OrderState, OrderEvent];
type AuthMachineBuilder = StateMachineBuilder[AuthState, AuthEvent];

// --- Factory Functions ---

func createHttpStateMachine() -> HttpStateMachine {
    StateMachine(initialState: HttpRequestState.idle())
}

func createOrderStateMachine() -> OrderStateMachine {
    StateMachine(initialState: OrderState.draft())
}

func createAuthStateMachine() -> AuthStateMachine {
    StateMachine(initialState: AuthState.anonymous())
}

func buildOrderWorkflow() -> OrderStateMachine {
    let builder: OrderMachineBuilder = StateMachineBuilder(startingWith: OrderState.draft())
    
    builder
        .addTransition(from: OrderState.draft(), to: OrderState.submitted(), on: "submit")
        .addTransition(from: OrderState.submitted(), to: OrderState.processing(), on: "process")
        .addTransition(from: OrderState.processing(), to: OrderState.shipped(), on: "ship")
        .addTransition(from: OrderState.shipped(), to: OrderState.delivered(), on: "deliver")
        .addTransition(from: OrderState.submitted(), to: OrderState.cancelled(), on: "cancel")
        .addTransition(from: OrderState.processing(), to: OrderState.cancelled(), on: "cancel")
        .addTransition(from: OrderState.delivered(), to: OrderState.refunded(), on: "refund")
        .build()
}

// --- Example Usage ---

func demonstrateStateMachine() {
    var orderMachine: OrderStateMachine = buildOrderWorkflow()
    orderMachine.start()
    
    let submitEvent: OrderEvent = OrderEvent.submit(orderId: "order-123")
    let processEvent: OrderEvent = OrderEvent.process(orderId: "order-123")
    let shipEvent: OrderEvent = OrderEvent.ship(orderId: "order-123")
    
    orderMachine.dispatch(event: submitEvent)
    orderMachine.dispatch(event: processEvent)
    orderMachine.dispatch(event: shipEvent)
    
    let currentState: OrderState = orderMachine.getCurrentState()
    let history: [StateTransitionRecord[OrderState, OrderEvent]] = orderMachine.getHistory()
    
    ()
}

func demonstrateAuthFlow() {
    var authMachine: AuthStateMachine = createAuthStateMachine()
    authMachine.start()
    
    let loginEvent: AuthEvent = AuthEvent.login(userId: "user-456")
    authMachine.dispatch(event: loginEvent)
    
    let mfaEvent: AuthEvent = AuthEvent.mfaChallenge()
    authMachine.dispatch(event: mfaEvent)
    
    let verifyEvent: AuthEvent = AuthEvent.mfaVerified()
    authMachine.dispatch(event: verifyEvent)
    
    let state: AuthState = authMachine.getCurrentState()
    let isLoggedIn: Bool = state.isLoggedIn()
    
    ()
}

