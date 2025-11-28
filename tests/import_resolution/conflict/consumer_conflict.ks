// This should produce a conflict - both modules have Widget
module ConflictConsumer

// Import Widget from both modules - potential conflict
import ModuleA.(Widget)
import ModuleB.(Widget)

class MyClass {}
