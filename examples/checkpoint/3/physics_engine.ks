module PhysicsEngine

// ============================================================================
// A 2D Physics Simulation Engine
// ============================================================================
// Demonstrates: floating-point operators, complex physics formulas,
// vector math with operators, and real-world calculations

// --- Constants ---

struct PhysicsConstants {
    static func gravity() -> Float { 9.81 }
    static func pi() -> Float { 3.14159265359 }
    static func tau() -> Float { 6.28318530718 }
    static func epsilon() -> Float { 0.0001 }
}

// --- 2D Vector ---

struct Vector2 {
    var x: Float
    var y: Float
    
    init(x: Float, y: Float) {
        self.x = x
        self.y = y
    }
    
    init() {
        self.x = 0.0
        self.y = 0.0
    }
    
    static func zero() -> Vector2 {
        Vector2(x: 0.0, y: 0.0)
    }
    
    static func one() -> Vector2 {
        Vector2(x: 1.0, y: 1.0)
    }
    
    static func up() -> Vector2 {
        Vector2(x: 0.0, y: 1.0)
    }
    
    static func down() -> Vector2 {
        Vector2(x: 0.0, y: -1.0)
    }
    
    static func left() -> Vector2 {
        Vector2(x: -1.0, y: 0.0)
    }
    
    static func right() -> Vector2 {
        Vector2(x: 1.0, y: 0.0)
    }
    
    // Vector addition
    func add(other: Vector2) -> Vector2 {
        Vector2(x: self.x + other.x, y: self.y + other.y)
    }
    
    // Vector subtraction
    func subtract(other: Vector2) -> Vector2 {
        Vector2(x: self.x - other.x, y: self.y - other.y)
    }
    
    // Scalar multiplication
    func scale(scalar: Float) -> Vector2 {
        Vector2(x: self.x * scalar, y: self.y * scalar)
    }
    
    // Scalar division
    func divide(scalar: Float) -> Vector2 {
        Vector2(x: self.x / scalar, y: self.y / scalar)
    }
    
    // Negation
    func negate() -> Vector2 {
        Vector2(x: -self.x, y: -self.y)
    }
    
    // Dot product
    func dot(other: Vector2) -> Float {
        self.x * other.x + self.y * other.y
    }
    
    // Cross product (returns scalar in 2D)
    func cross(other: Vector2) -> Float {
        self.x * other.y - self.y * other.x
    }
    
    // Magnitude squared (avoids sqrt)
    func magnitudeSquared() -> Float {
        self.x * self.x + self.y * self.y
    }
    
    // Distance squared between two points
    func distanceSquaredTo(other: Vector2) -> Float {
        let dx: Float = other.x - self.x
        let dy: Float = other.y - self.y
        dx * dx + dy * dy
    }
    
    // Linear interpolation
    func lerp(target: Vector2, t: Float) -> Vector2 {
        let oneMinusT: Float = 1.0 - t
        let newX: Float = self.x * oneMinusT + target.x * t
        let newY: Float = self.y * oneMinusT + target.y * t
        Vector2(x: newX, y: newY)
    }
    
    // Reflect vector off a normal
    func reflect(normal: Vector2) -> Vector2 {
        let dotProduct: Float = self.dot(normal)
        let factor: Float = 2.0 * dotProduct
        let reflectedX: Float = self.x - factor * normal.x
        let reflectedY: Float = self.y - factor * normal.y
        Vector2(x: reflectedX, y: reflectedY)
    }
    
    // Check if approximately zero
    func isNearZero() -> Bool {
        let eps: Float = PhysicsConstants.epsilon()
        self.x * self.x + self.y * self.y < eps * eps
    }
    
    // Check equality with epsilon
    func approximatelyEquals(other: Vector2) -> Bool {
        let dx: Float = self.x - other.x
        let dy: Float = self.y - other.y
        let distSq: Float = dx * dx + dy * dy
        let eps: Float = PhysicsConstants.epsilon()
        distSq < eps * eps
    }
}

// --- Rigid Body ---

struct RigidBody {
    var position: Vector2
    var velocity: Vector2
    var acceleration: Vector2
    var mass: Float
    var restitution: Float  // Bounciness (0-1)
    var friction: Float     // Surface friction (0-1)
    var isStatic: Bool
    
    init(at position: Vector2, withMass mass: Float) {
        self.position = position
        self.velocity = Vector2.zero()
        self.acceleration = Vector2.zero()
        self.mass = mass
        self.restitution = 0.5
        self.friction = 0.3
        self.isStatic = false
    }
    
    init(staticAt position: Vector2) {
        self.position = position
        self.velocity = Vector2.zero()
        self.acceleration = Vector2.zero()
        self.mass = 0.0
        self.restitution = 0.5
        self.friction = 0.3
        self.isStatic = true
    }
    
    // Apply force: F = ma, so a = F/m
    mutating func applyForce(force: Vector2) {
        let ax: Float = force.x / self.mass
        let ay: Float = force.y / self.mass
        self.acceleration = Vector2(x: self.acceleration.x + ax, y: self.acceleration.y + ay)
    }
    
    // Apply impulse (instant velocity change)
    mutating func applyImpulse(impulse: Vector2) {
        let dvx: Float = impulse.x / self.mass
        let dvy: Float = impulse.y / self.mass
        self.velocity = Vector2(x: self.velocity.x + dvx, y: self.velocity.y + dvy)
    }
    
    // Integrate motion using Euler method
    mutating func integrate(dt: Float) {
        // Update velocity: v = v + a * dt
        let newVx: Float = self.velocity.x + self.acceleration.x * dt
        let newVy: Float = self.velocity.y + self.acceleration.y * dt
        self.velocity = Vector2(x: newVx, y: newVy)
        
        // Update position: p = p + v * dt
        let newPx: Float = self.position.x + self.velocity.x * dt
        let newPy: Float = self.position.y + self.velocity.y * dt
        self.position = Vector2(x: newPx, y: newPy)
        
        // Reset acceleration for next frame
        self.acceleration = Vector2.zero()
    }
    
    // Calculate kinetic energy: KE = 0.5 * m * v²
    func kineticEnergy() -> Float {
        let speedSquared: Float = self.velocity.magnitudeSquared()
        0.5 * self.mass * speedSquared
    }
    
    // Calculate momentum: p = m * v
    func momentum() -> Vector2 {
        Vector2(x: self.mass * self.velocity.x, y: self.mass * self.velocity.y)
    }
    
    // Check if moving
    func isMoving() -> Bool {
        not self.velocity.isNearZero()
    }
}

// --- Circle Collider ---

struct CircleCollider {
    var center: Vector2
    var radius: Float
    
    func containsPoint(point: Vector2) -> Bool {
        let distSq: Float = center.distanceSquaredTo(point)
        distSq <= radius * radius
    }
    
    func intersectsCircle(other: CircleCollider) -> Bool {
        let distSq: Float = self.center.distanceSquaredTo(other.center)
        let radiusSum: Float = self.radius + other.radius
        distSq <= radiusSum * radiusSum
    }
    
    func area() -> Float {
        PhysicsConstants.pi() * self.radius * self.radius
    }
    
    func circumference() -> Float {
        PhysicsConstants.tau() * self.radius
    }
}

// --- Axis-Aligned Bounding Box ---

struct AABB {
    var minX: Float
    var minY: Float
    var maxX: Float
    var maxY: Float
    
    init(center: Vector2, halfWidth: Float, halfHeight: Float) {
        self.minX = center.x - halfWidth
        self.minY = center.y - halfHeight
        self.maxX = center.x + halfWidth
        self.maxY = center.y + halfHeight
    }
    
    func width() -> Float {
        self.maxX - self.minX
    }
    
    func height() -> Float {
        self.maxY - self.minY
    }
    
    func area() -> Float {
        (self.maxX - self.minX) * (self.maxY - self.minY)
    }
    
    func center() -> Vector2 {
        let cx: Float = (self.minX + self.maxX) / 2.0
        let cy: Float = (self.minY + self.maxY) / 2.0
        Vector2(x: cx, y: cy)
    }
    
    func containsPoint(point: Vector2) -> Bool {
        point.x >= self.minX and point.x <= self.maxX and
        point.y >= self.minY and point.y <= self.maxY
    }
    
    func intersectsAABB(other: AABB) -> Bool {
        self.minX <= other.maxX and self.maxX >= other.minX and
        self.minY <= other.maxY and self.maxY >= other.minY
    }
}

// --- Physics Formulas ---

// Gravitational force between two masses
func gravitationalForce(m1: Float, m2: Float, distance: Float) -> Float {
    let G: Float = 6.674  // Simplified gravitational constant
    G * m1 * m2 / (distance * distance)
}

// Spring force (Hooke's Law): F = -k * x
func springForce(stiffness: Float, displacement: Float) -> Float {
    -stiffness * displacement
}

// Drag force: F = 0.5 * rho * v² * Cd * A
func dragForce(density: Float, velocity: Float, dragCoeff: Float, area: Float) -> Float {
    0.5 * density * velocity * velocity * dragCoeff * area
}

// Centripetal acceleration: a = v² / r
func centripetalAcceleration(velocity: Float, radius: Float) -> Float {
    velocity * velocity / radius
}

// Orbital velocity: v = sqrt(G * M / r) - simplified
func orbitalVelocitySquared(centralMass: Float, radius: Float) -> Float {
    let G: Float = 6.674
    G * centralMass / radius
}

// Time of flight for projectile (simplified, no air resistance)
func projectileTimeOfFlight(initialVelocityY: Float, gravity: Float) -> Float {
    2.0 * initialVelocityY / gravity
}

// Maximum height of projectile
func projectileMaxHeight(initialVelocityY: Float, gravity: Float) -> Float {
    initialVelocityY * initialVelocityY / (2.0 * gravity)
}

// Range of projectile (horizontal distance)
func projectileRange(initialVelocity: Float, angle: Float, gravity: Float) -> Float {
    // Simplified: assumes sin(2*angle) ≈ 1 for 45 degrees
    initialVelocity * initialVelocity / gravity
}

// --- Collision Response ---

struct CollisionResult {
    let collided: Bool
    let penetrationDepth: Float
    let normalX: Float
    let normalY: Float
}

func resolveCircleCollision(a: CircleCollider, b: CircleCollider) -> CollisionResult {
    let dx: Float = b.center.x - a.center.x
    let dy: Float = b.center.y - a.center.y
    let distSq: Float = dx * dx + dy * dy
    let radiusSum: Float = a.radius + b.radius
    let radiusSumSq: Float = radiusSum * radiusSum
    
    let collided: Bool = distSq < radiusSumSq
    
    // Simplified - would need sqrt for proper normal
    CollisionResult(
        collided: collided,
        penetrationDepth: radiusSum,
        normalX: dx,
        normalY: dy
    )
}

// Calculate impulse magnitude for elastic collision
func collisionImpulse(relativeVelocity: Float, restitution: Float, invMassSum: Float) -> Float {
    -(1.0 + restitution) * relativeVelocity / invMassSum
}

// --- Simulation Step ---

func simulateStep(body: RigidBody, gravity: Vector2, dt: Float) -> RigidBody {
    var result: RigidBody = body
    
    // Apply gravity
    let gravityForceX: Float = gravity.x * result.mass
    let gravityForceY: Float = gravity.y * result.mass
    let gravityForce: Vector2 = Vector2(x: gravityForceX, y: gravityForceY)
    result.applyForce(force: gravityForce)
    
    // Integrate
    result.integrate(dt: dt)
    
    result
}

// --- Demo ---

func runPhysicsDemo() {
    // Create a ball
    let startPos: Vector2 = Vector2(x: 0.0, y: 10.0)
    var ball: RigidBody = RigidBody(at: startPos, withMass: 1.0)
    
    // Gravity vector
    let gravity: Vector2 = Vector2(x: 0.0, y: -9.81)
    
    // Time step
    let dt: Float = 0.016  // ~60 FPS
    
    // Simulate one step
    ball = simulateStep(body: ball, gravity: gravity, dt: dt)
    
    // Check energy
    let ke: Float = ball.kineticEnergy()
    let pe: Float = ball.mass * 9.81 * ball.position.y
    let totalEnergy: Float = ke + pe
    
    // Vector operations
    let v1: Vector2 = Vector2(x: 3.0, y: 4.0)
    let v2: Vector2 = Vector2(x: 1.0, y: 2.0)
    
    let sum: Vector2 = v1.add(v2)
    let diff: Vector2 = v1.subtract(v2)
    let scaled: Vector2 = v1.scale(scalar: 2.0)
    let dot: Float = v1.dot(v2)
    let cross: Float = v1.cross(v2)
    
    // Collision detection
    let c1: CircleCollider = CircleCollider(center: Vector2(x: 0.0, y: 0.0), radius: 1.0)
    let c2: CircleCollider = CircleCollider(center: Vector2(x: 1.5, y: 0.0), radius: 1.0)
    let collides: Bool = c1.intersectsCircle(other: c2)
    
    // AABB test
    let box: AABB = AABB(center: Vector2(x: 5.0, y: 5.0), halfWidth: 2.0, halfHeight: 2.0)
    let point: Vector2 = Vector2(x: 5.5, y: 5.5)
    let inside: Bool = box.containsPoint(point: point)
    
    ()
}

