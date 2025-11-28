// This should NOT conflict - use aliases to disambiguate
module AliasedConsumer

// Import Widget from both modules with aliases
import ModuleA.(Widget as WidgetA)
import ModuleB.(Widget as WidgetB)

class MyClass {}
