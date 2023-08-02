# Running mediation and moderator analysis in Process 
set.seed(8189)
process(data = data.tg,
        y = "BSSIScore2",
        x = "SCTotalScore",
        m = "AKUADSscore",
        model = 4)

# more options in action
set.seed(8189)
process(data = data.tg,
        y = "BSSIScore2",
        x = "SCTotalScore",
        m = "AKUADSscore",
        model = 4,
        effsize = 1,
        total = 1,
        stand = 1,
        boot = 10000)
