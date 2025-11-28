// This should conflict with a local declaration
module LocalConflict

import ModuleA.(Widget)

// This local class has the same name as the imported Widget
class Widget {}
