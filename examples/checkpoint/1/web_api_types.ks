module WebAPI

// HTTP request/response types for a simple web API

// Represent HTTP request headers
struct Headers {
    let contentType: String
    let authorization: String
    let userAgent: String

    func getContentType() -> String { }
    func isAuthorized() -> Bool { }
}

// Represent an HTTP request
struct Request {
    let method: String
    let path: String
    let headers: Headers
    let body: String

    func getPath() -> String { }
    func isPost() -> Bool { }
    func getHeaders() -> Headers { }
}

// Represent an HTTP response
struct Response {
    let statusCode: Int
    let statusMessage: String
    let headers: Headers
    let body: String

    func isSuccess() -> Bool { }
    func isError() -> Bool { }
    func getBody() -> String { }
}

// Generic API response that can wrap any data
struct ApiResponse[T] {
    let success: Bool
    let data: T
    let error: String
    let timestamp: Int

    func isSuccessful() -> Bool { }
    func getData() -> T { }
}

// User data structure for API
struct User {
    let id: Int
    let username: String
    let email: String
    let roles: [String]

    func getId() -> Int { }
    func getUsername() -> String { }
    func hasRole(role: String) -> Bool { }
}

// Product data for e-commerce API
struct Product {
    let id: String
    let name: String
    let price: Float
    let quantity: Int
    let tags: [String]

    func getId() -> String { }
    func getPrice() -> Float { }
    func isInStock() -> Bool { }
}

// Order containing multiple products
struct Order {
    let orderId: String
    let userId: Int
    let items: [Product]
    let total: Float
    let status: String

    func getOrderId() -> String { }
    func getTotal() -> Float { }
    func getItemCount() -> Int { }
}

// Authentication token
struct Token {
    let accessToken: String
    let tokenType: String
    let expiresIn: Int
    let refreshToken: String

    func getAccessToken() -> String { }
    func isExpired() -> Bool { }
}

// Pagination metadata for list responses
struct PaginationMetadata {
    let currentPage: Int
    let pageSize: Int
    let totalItems: Int
    let totalPages: Int

    func getCurrentPage() -> Int { }
    func hasNextPage() -> Bool { }
    func hasPreviousPage() -> Bool { }
}

// Protocol for resources that can be serialized to JSON
protocol JsonSerializable {
    func toJson() -> String
    func fromJson(json: String) -> Self
}

// A paginated list response
struct PaginatedResponse[T] {
    let data: [T]
    let pagination: PaginationMetadata

    func getData() -> [T] { }
    func getPagination() -> PaginationMetadata { }
}

// Create sample API data
func createSampleUser() -> User {
    let roles: [String] = ["user", "admin"]
    ("user-123", "john_doe", "john@example.com", roles)
}

func createSampleProduct() -> Product {
    let tags: [String] = ["electronics", "gadgets", "tech"]
    ("prod-456", "Wireless Headphones", 79.99, 150, tags)
}

func createSampleOrder() -> Order {
    let products: [Product] = [
        ("prod-1", "Mouse", 29.99, 2, []),
        ("prod-2", "Keyboard", 79.99, 1, [])
    ]
    ("order-789", 123, products, 139.97, "shipped")
}

func createSuccessResponse[T](data: T) -> ApiResponse[T] {
    (true, data, "", 1701619200)
}

func createErrorResponse[T](error: String) -> ApiResponse[T] {
    (false, error, error, 1701619200)
}
