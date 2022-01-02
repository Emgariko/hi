module HW3.Base (
    HiFun,
    HiValue,
    HiExpr,
    HiError
    ) where

data HiFun     -- function names (e.g. div, sort, length, ...)
data HiValue   -- values (numbers, booleans, strings, ...)
data HiExpr    -- expressions (literals, function calls, ...)
data HiError   -- evaluation errors (invalid arguments, ...)