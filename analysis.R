
library(caret)

ggplot(df, aes(x = rank, y = score)) + geom_point()

ggplot(df, aes(x = diff, y = score)) + geom_point()



df_exp <- mutate(df, diff_exp = exp(diff/100))
df_exp %<>%
        select(c(-2))


df_qua <- mutate(df, diff_qua = diff*diff)



lm1 <- train(score ~., data = df, method = "lm")
summary(lm1$finalModel)

lm2 <- train(score ~., data = df_exp, method = "lm")
summary(lm2$finalModel)

lm3 <- train(score ~., data = df_qua, method = "lm")
summary(lm3$finalModel)


