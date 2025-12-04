module GameEngine

// ============================================================================
// A 2D Game Engine Foundation
// ============================================================================
// Demonstrates: structs, protocols, generics, initializers, methods,
// mutability, protocol conformance, nested types, and function overloading

// --- Core Math Types ---

struct Vec2 {
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

    func magnitude() -> Float { }
    func normalized() -> Vec2 { }
    func dot(other: Vec2) -> Float { }
}

struct Rect {
    var origin: Vec2
    var size: Vec2

    init(x: Float, y: Float, width: Float, height: Float) {
        self.origin = Vec2(x: x, y: y)
        self.size = Vec2(x: width, y: height)
    }

    func contains(point: Vec2) -> Bool { }
    func intersects(other: Rect) -> Bool { }
    func center() -> Vec2 { }
}

struct Color {
    let r: Int
    let g: Int
    let b: Int
    let a: Int

    init(r: Int, g: Int, b: Int) {
        self.r = r
        self.g = g
        self.b = b
        self.a = 255
    }

    init(r: Int, g: Int, b: Int, a: Int) {
        self.r = r
        self.g = g
        self.b = b
        self.a = a
    }

    static func red() -> Color { Color(r: 255, g: 0, b: 0) }
    static func green() -> Color { Color(r: 0, g: 255, b: 0) }
    static func blue() -> Color { Color(r: 0, g: 0, b: 255) }
    static func white() -> Color { Color(r: 255, g: 255, b: 255) }
    static func black() -> Color { Color(r: 0, g: 0, b: 0) }
}

// --- Core Protocols ---

protocol Renderable {
    func render()
    func getBounds() -> Rect
}

protocol Updateable {
    func update(deltaTime: Float)
}

protocol Collidable {
    func getBoundingBox() -> Rect
    func onCollision(other: Collidable)
}

// --- Entity Component System ---

struct Transform {
    var position: Vec2
    var rotation: Float
    var scale: Vec2

    init() {
        self.position = Vec2()
        self.rotation = 0.0
        self.scale = Vec2(x: 1.0, y: 1.0)
    }

    init(at position: Vec2) {
        self.position = position
        self.rotation = 0.0
        self.scale = Vec2(x: 1.0, y: 1.0)
    }

    mutating func translate(by delta: Vec2) { }
    mutating func rotate(by angle: Float) { }
    mutating func scaleBy(factor: Float) { }
}

struct Sprite: Renderable {
    let textureId: String
    var color: Color
    var transform: Transform

    init(texture: String) {
        self.textureId = texture
        self.color = Color.white()
        self.transform = Transform()
    }

    func render() { }
    func getBounds() -> Rect { }
}

// --- Generic Component Container ---

struct Component[T] {
    let id: Int
    var data: T
    var enabled: Bool

    init(id: Int, data: T) {
        self.id = id
        self.data = data
        self.enabled = true
    }

    func isEnabled() -> Bool { self.enabled }
    mutating func enable() { self.enabled = true }
    mutating func disable() { self.enabled = false }
}

// --- Game Entities ---

struct Entity {
    let id: Int
    let name: String
    var transform: Transform
    var active: Bool

    init(id: Int, name: String) {
        self.id = id
        self.name = name
        self.transform = Transform()
        self.active = true
    }

    func getId() -> Int { self.id }
    func getName() -> String { self.name }
    func isActive() -> Bool { self.active }
}

struct Player: Renderable, Updateable, Collidable {
    var entity: Entity
    var sprite: Sprite
    var health: Int
    var speed: Float
    var velocity: Vec2

    init(id: Int, name: String) {
        self.entity = Entity(id: id, name: name)
        self.sprite = Sprite(texture: "player.png")
        self.health = 100
        self.speed = 200.0
        self.velocity = Vec2()
    }

    func render() { }
    func getBounds() -> Rect { }
    func update(deltaTime: Float) { }
    func getBoundingBox() -> Rect { }
    func onCollision(other: Collidable) { }

    func isAlive() -> Bool { }
    mutating func takeDamage(amount: Int) { }
    mutating func heal(amount: Int) { }
}

struct Enemy: Renderable, Updateable, Collidable {
    var entity: Entity
    var sprite: Sprite
    var health: Int
    var damage: Int
    var patrolPath: [Vec2]

    init(id: Int, enemyType: String) {
        self.entity = Entity(id: id, name: enemyType)
        self.sprite = Sprite(texture: "enemy.png")
        self.health = 50
        self.damage = 10
        self.patrolPath = []
    }

    func render() { }
    func getBounds() -> Rect { }
    func update(deltaTime: Float) { }
    func getBoundingBox() -> Rect { }
    func onCollision(other: Collidable) { }
}

struct Projectile: Renderable, Updateable {
    var entity: Entity
    var sprite: Sprite
    var velocity: Vec2
    var damage: Int
    var lifetime: Float

    init(id: Int, at position: Vec2, withVelocity velocity: Vec2) {
        self.entity = Entity(id: id, name: "projectile")
        self.entity.transform = Transform(at: position)
        self.sprite = Sprite(texture: "bullet.png")
        self.velocity = velocity
        self.damage = 25
        self.lifetime = 5.0
    }

    func render() { }
    func getBounds() -> Rect { }
    func update(deltaTime: Float) { }

    func isExpired() -> Bool { }
}

// --- Scene Management ---

struct Scene {
    let name: String
    var entities: [Entity]
    var isLoaded: Bool

    init(name: String) {
        self.name = name
        self.entities = []
        self.isLoaded = false
    }

    func getName() -> String { self.name }
    mutating func load() { self.isLoaded = true }
    mutating func unload() { self.isLoaded = false }
}

struct GameState {
    var currentScene: Scene
    var score: Int
    var isPaused: Bool
    var gameTime: Float

    init(initialScene: Scene) {
        self.currentScene = initialScene
        self.score = 0
        self.isPaused = false
        self.gameTime = 0.0
    }

    mutating func pause() { self.isPaused = true }
    mutating func resume() { self.isPaused = false }
    mutating func addScore(points: Int) { }
}

// --- Input System ---

protocol InputHandler {
    func handleKeyDown(keyCode: Int)
    func handleKeyUp(keyCode: Int)
    func handleMouseMove(position: Vec2)
    func handleMouseClick(button: Int, position: Vec2)
}

struct KeyState {
    let keyCode: Int
    var isPressed: Bool
    var justPressed: Bool
    var justReleased: Bool

    init(keyCode: Int) {
        self.keyCode = keyCode
        self.isPressed = false
        self.justPressed = false
        self.justReleased = false
    }
}

// --- Audio System ---

struct AudioClip {
    let id: String
    let path: String
    let duration: Float
    var volume: Float

    init(id: String, path: String, duration: Float) {
        self.id = id
        self.path = path
        self.duration = duration
        self.volume = 1.0
    }
}

struct AudioSource {
    var clip: AudioClip
    var isPlaying: Bool
    var isLooping: Bool
    var position: Vec2

    init(clip: AudioClip) {
        self.clip = clip
        self.isPlaying = false
        self.isLooping = false
        self.position = Vec2()
    }

    mutating func play() { self.isPlaying = true }
    mutating func stop() { self.isPlaying = false }
    mutating func setLoop(looping: Bool) { self.isLooping = looping }
}

// --- Factory Functions ---

func createPlayer(withId id: Int, named name: String) -> Player {
    Player(id: id, name: name)
}

func createEnemy(withId id: Int, ofType enemyType: String) -> Enemy {
    Enemy(id: id, enemyType: enemyType)
}

func createProjectile(withId id: Int, at position: Vec2, direction: Vec2, speed: Float) -> Projectile {
    let velocity: Vec2 = Vec2(x: 0.0, y: 0.0)
    Projectile(id: id, at: position, withVelocity: velocity)
}

func createScene(named name: String) -> Scene {
    Scene(name: name)
}

// --- Sample Game Setup ---

func setupDemoGame() -> GameState {
    let mainScene: Scene = createScene(named: "MainLevel")
    let state: GameState = GameState(initialScene: mainScene)
    
    let player: Player = createPlayer(withId: 1, named: "Hero")
    let enemy1: Enemy = createEnemy(withId: 2, ofType: "Goblin")
    let enemy2: Enemy = createEnemy(withId: 3, ofType: "Skeleton")
    
    state
}

