module State where
import           Control.Arrow (first)

type S = Int
type SM a = S -> (a, S)

smap :: (a->b) -> (SM a -> SM b)
smap f t = first f . t

spure :: a -> SM a
spure a s = (a, s)
-- spure = (,)

sbind :: SM a -> (a -> SM b) -> SM b
sbind f k = \s -> let (a,s') = f s in k a s'

sjoin :: SM (SM a) -> SM a
-- uncurry ($) :: (b -> c, b) -> c
sjoin mma = uncurry ($) . mma


type CS s a r = (a -> s -> r) -> s -> r

csmap :: (a -> b) -> CS s a r -> CS s b r
-- cmap :: (a -> b) -> ((a -> s -> r) -> s -> r) -> ((b -> s -> r) -> s -> r)
csmap f cas cb = cas (cb . f)

cspure :: a -> CS s a r
cspure = flip ($)

csbind :: CS s a r -> (a -> CS s b r) -> CS s b r
-- cbind :: ((a -> s -> r) -> s -> r) -> (a -> (b -> s -> r) -> s -> r) -> ((b -> s -> r) -> s -> r)
csbind cas fcbs cb = cas (\a -> fcbs a cb)

csthen :: CS s a r -> CS s b r -> CS s b r
csthen cas cbs = csbind cas (const cbs)

foo :: CS s Int r
foo = csmap (+1) (cspure 41)

test1eq42 = foo (\a _ -> a) 17

csget :: CS s s r
csget cs s = cs s s

csput :: s -> CS s () r
-- csput :: s -> (s -> () -> r) -> s -> r
csput s' cu _ = cu () s'

csmodify :: (s -> s) -> CS s () r
csmodify t = csget `csbind` (\s -> csput (t s))

cstick :: CS Int () r
cstick = csmodify (+1)

bar :: CS Int Int r
bar = csput 40 `csthen` cstick `csthen` cstick `csthen` csget

test2eq42 = bar const 0

