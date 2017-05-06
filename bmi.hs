bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "你太瘦了，火柴人！"
    | bmi <= normal = "你体重应该是正常的。呼呼，我想你一定是个丑逼！"
    | bmi <= fat    = "你太胖了！该减肥了！死胖子！"
    | otherwise   = "恭喜你已经胖成鲸鱼🐳了！"
    where bmi    = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat    = 30.0

{-bmiTell :: (RealFloat a) => a -> a -> String-}
{-bmiTell weight height-}
    {-| bmi <= skinny = "You're underweight, you emo, you!"-}
    {-| bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"-}
    {-| bmi <= fat    = "You're fat! Lose some weight, fatty!"-}
    {-| otherwise   = "You're a whale, congratulations!"-}
    {-where bmi    = weight / height ^ 2-}
          {-skinny = 18.5-}
          {-normal = 25.0-}
          {-fat    = 30.0-}

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  

main = do
    putStrLn "输入你的体重（kg）："
    w <- getLine
    putStrLn "输入你的身高（m）："
    h <- getLine
    putStrLn $ bmiTell (read w :: Float)  (read h :: Float)

    putStrLn ""
    main
