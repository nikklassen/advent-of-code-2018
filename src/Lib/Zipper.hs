{--
Copyright (c) 2008, Ryan Ingram
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

Modifications:
    - Modified to use Data.Sequence instead of Lists
    - Unused functions were removed
--}
module Lib.Zipper where

import Data.Sequence
import Data.Foldable (toList)

data Zipper a = Zip (Seq a) (Seq a) deriving (Eq,Show)

-- | @fromList xs@ returns a zipper containing the elements of xs,
-- focused on the first element.
fromList :: [a] -> Zipper a
fromList as = Zip empty (Data.Sequence.fromList as)

toList :: Zipper a -> [a]
toList (Zip ls rs) = Data.Foldable.toList $ ls >< rs

-- | @beginp z@ returns @True@ if the zipper is at the start.
beginp :: Zipper a -> Bool
beginp (Zip Empty _ ) = True
beginp _           = False

-- | @endp z@ returns @True@ if the zipper is at the end.
-- It is not safe to call @cursor@ on @z@ if @endp z@ returns @True@.
endp :: Zipper a -> Bool
endp   (Zip _  Empty) = True
endp   _           = False

start, end :: Zipper a -> Zipper a
start (Zip ls rs) = Zip empty (ls >< rs)
end   (Zip ls rs) = Zip (ls >< rs) empty

-- | @cursor z@ returns the targeted element in @z@.
--
-- This function is not total, but the invariant is that
-- @endp z == False@ means that you can safely call
-- @cursor z@.
cursor :: Zipper a -> a
cursor (Zip _ (a:<|_)) = a

-- | @left z@ returns the zipper with the focus
-- shifted left one element.
left :: Zipper a -> Zipper a
left  (Zip (ls:|>a) rs) = Zip ls (a<|rs)
left  z               = z

leftWrap :: Zipper a -> Zipper a
leftWrap  (Zip (ls:|>a) rs) = Zip ls (a<|rs)
leftWrap  (Zip empty (rs:|>a)) = Zip rs (singleton a)

-- | @right z@ returns the zipper with the focus
-- shifted right one element; this can move the
-- cursor off the end.
right :: Zipper a -> Zipper a
right (Zip ls (a:<|rs)) = Zip (ls|>a) rs
right z               = z

rightWrap :: Zipper a -> Zipper a
rightWrap (Zip ls (a:<|rs)) = Zip (ls|>a) rs
rightWrap (Zip (a:<|ls) empty) = Zip (singleton a) ls

-- | @insert x z@ adds x at the cursor.
insert :: a -> Zipper a -> Zipper a
insert a (Zip ls rs) = Zip ls (a<|rs)

-- | @delete z@ removes the element at the cursor (if any).
-- Safe to call on an empty zipper.
-- forall x z. delete (insert x z) == z
delete :: Zipper a -> Zipper a
delete (Zip ls (_:<|rs)) = Zip ls rs
delete z               = z

-- | @replace a z@ changes the current element in the zipper
-- to the passed in value.  If there is no current element,
-- the zipper is unchanged.  If you want to add the element
-- in that case instead, use @insert a (delete z)@.
replace :: a -> Zipper a -> Zipper a
replace a (Zip ls (_:<|rs)) = Zip ls (a<|rs)
replace _ z               = z