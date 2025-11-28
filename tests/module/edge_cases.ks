// Edge cases for module declarations

// Very long module path
module A.B.C.D.E.F.G.H.I.J.K.L.M.N.O.P

// Module with underscores in names
module My_Module.Sub_Module

// Single segment module (minimal valid)
module Main

// Single character segments
module A.B.C.D.E

// Mixed case segments
module MyApp.UserModule.AuthController

// Unicode identifiers and very long names are tested in edge_cases.ks
