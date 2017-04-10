module Lib (parsePIPs, runScript, PValue(PError, PAssignScope), defaultScope, unmeta, Scope, ASTNode, mergeScopes) where

import PIPsParser
import AST
import Terp
import Runtime
import StdLib

