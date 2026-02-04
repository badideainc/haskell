heartMonitor :: Int -> Int -> String
heartMonitor age bpm
    | age > 80 && bpm > 100 = "High heart rate for 81+!"
    | age > 60 && bpm > 130 = "High heart rate for 61-80!"
    | age > 40 && bpm > 140 = "High heart rate for 41-60!"
    | age > 20 && bpm > 155 = "High heart rate for 21-40!"
    | age >= 0 && bpm > 170 = "High heart rate for 0-20!"
    | otherwise = "Normal heart rate"