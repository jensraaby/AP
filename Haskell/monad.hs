-- (>>==) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- x (>>==) f = case x of

--     Nothing -> Nothing
--     Just n  -> f n  
-- return :: a -> Maybe a
-- return x = Just x
--     -- not return x = Nothing
--     -- not return x = x
--     
checkedAdd x y = do
  m <- x
  n <- y
  return $ m+n
  
