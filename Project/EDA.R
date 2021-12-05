
exercise = read_csv("/Users/andyc/Desktop/Masters/MDA 9159 Stat modelling/Project/490data1.csv")
exercise$student_id = as.factor(exercise$student_id)
exercise$age = as.factor(exercise$age)
exercise$day = as.factor(exercise$day)
library(ggplot2)

# Summary & Descriptive statistics
summary(exercise)

# Checking for outliers
boxplot.stats(exercise$systolic)$out
boxplot.stats(exercise$diastolic)$out
boxplot.stats(exercise$heart_rate)$out

# Normality checking
qqnorm(exercise$systolic); qqline(exercise$systolic)
qqnorm(exercise$diastolic); qqline(exercise$diastolic)
qqnorm(exercise$heart_rate); qqline(exercise$heart_rate)


# ggplots

# Dist'n of sex 
ggplot(data = exercise, aes(sex,..count..)) + geom_bar(aes(fill=sex))+facet_grid(~day)

# Trends
ggplot(data = exercise, aes(x=day, y = systolic, group = student_id, shape = student_id)) + geom_point() + geom_line()
ggplot(data = exercise, aes(x=day, y = diastolic, group = student_id, shape = student_id)) + geom_point() + geom_line()
ggplot(data = exercise, aes(x=day, y = heart_rate, group = student_id, shape = student_id)) + geom_point() + geom_line()

# Systolic BP
ggplot(data = exercise, aes(x=day, y = systolic)) + geom_point() 
ggplot(data = exercise, aes(x=day, y = systolic)) + geom_boxplot()

# Diastolic BP 
ggplot(data = exercise, aes(x=day, y = diastolic)) + geom_point() 
ggplot(data = exercise, aes(x=day, y = diastolic)) + geom_boxplot()

# Heart rate
ggplot(data = exercise, aes(x=day, y = heart_rate)) + geom_point() 
ggplot(data = exercise, aes(x=day, y = heart_rate)) + geom_boxplot()

# Age and BP
ggplot(data = exercise, aes(x=age, y = systolic)) + geom_boxplot()
ggplot(data = exercise, aes(x=age, y = diastolic)) + geom_boxplot()
ggplot(data = exercise, aes(x=age, y = heart_rate)) + geom_boxplot()

# Sex and BP
ggplot(data = exercise, aes(x=sex, y = systolic)) + geom_boxplot()
ggplot(data = exercise, aes(x=sex, y = diastolic)) + geom_boxplot()
ggplot(data = exercise, aes(x=sex, y = heart_rate)) + geom_boxplot()

# Systolic vs diastolic
ggplot(data = exercise, aes(x=systolic, y = diastolic, colour=day)) + geom_point(size=2) + geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=systolic, y = diastolic, colour=day)) + geom_point(size=2)+ facet_grid(~sex) + geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=systolic, y = diastolic, colour=day)) + geom_point(size=2)+ facet_grid(~age) + geom_smooth( method=lm,color = "red")

# Exercise hr vs avg sleep hr 
ggplot(data = exercise, aes(x=exercise_hr, y = avg_sleep, colour=day)) + geom_point(size=2)+ geom_smooth( color = "red")
ggplot(data = exercise, aes(x=exercise_hr, y = avg_sleep, colour=day)) + geom_point(size=2)+ facet_grid(~sex) + geom_smooth( color = "red")
ggplot(data = exercise, aes(x=exercise_hr, y = avg_sleep, colour=day)) + geom_point(size=2)+ facet_grid(~age) + geom_smooth( color = "red")
#FINAL OUTPUT .A.C
ggplot(data = exercise, aes(x=exercise_hr, y = avg_sleep)) + geom_point(size=2)+ facet_grid(~sex) + geom_smooth(method = lm, color = "red")+labs(x="Hour of Exercises",y="Average sleep", title = 'Exercise to Sleep')


# Discipline vs adequate sleep
ggplot(data = exercise, aes(x=sleep_same_time, y = avg_sleep, colour=day)) + geom_point(size=2)+ geom_smooth( color = "red")
ggplot(data = exercise, aes(x=sleep_same_time, y = avg_sleep, colour=day)) + geom_point(size=2)+ facet_grid(~sex) + geom_smooth( color = "red")
ggplot(data = exercise, aes(x=sleep_same_time, y = avg_sleep, colour=day)) + geom_point(size=2)+ facet_grid(~age) + geom_smooth( color = "red")

# Relationship between # of midterms and exp score 
ggplot(data = exercise, aes(x=num_midterms, y = exp_mark, colour=day)) + geom_point(size=2)+ geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=num_midterms, y = exp_mark, colour=day)) + geom_point(size=2)+ facet_grid(~sex) + geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=num_midterms, y = exp_mark, colour=day)) + geom_point(size=2)+ facet_grid(~age) + geom_smooth( method=lm,color = "red")
#final output .A.C
ggplot(data = exercise, aes(x=num_midterms, y = exp_mark, colour=sex)) + geom_point(size=2)+ facet_grid(~sex) + geom_smooth( method=lm,color = "red")+labs(x="Number of Midterms",y="Expected Marks", title = 'Midterms Versus Expected Marks')

# Relationship between exercise hours and mental health score
ggplot(data = exercise, aes(x=exercise_hr, y = MH_score, colour=day)) + geom_point(size=2)+ geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=exercise_hr, y = MH_score, colour=day)) + geom_point(size=2)+ facet_grid(~sex) + geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=exercise_hr, y = MH_score, colour=day)) + geom_point(size=2)+ facet_grid(~age) + geom_smooth( method=lm,color = "red")
# FINAL OUTPUT 
ggplot(data = exercise, aes(x=exercise_hr, y = MH_score)) + geom_point(size=2)+facet_grid(~sex) + geom_smooth( method=lm,color = "red")+labs(x="Hour of Exercise",y="Mental Health Score", title = 'Exercise to Mental Health')



# Relationship between exercise hours and exp score
ggplot(data = exercise, aes(x=exercise_hr, y = exp_mark, colour=day)) + geom_point(size=2)+ geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=exercise_hr, y = exp_mark, colour=day)) + geom_point(size=2)+ facet_grid(~sex) + geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=exercise_hr, y = exp_mark, colour=day)) + geom_point(size=2)+ facet_grid(~age) + geom_smooth( method=lm,color = "red")

# Relationship between physical score and exp score 
ggplot(data = exercise, aes(x=phys_score, y = exp_mark, colour=day)) + geom_point(size=2)+ geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=phys_score, y = exp_mark, colour=day)) + geom_point(size=2)+ facet_grid(~sex) + geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=phys_score, y = exp_mark, colour=day)) + geom_point(size=2)+ facet_grid(~age) + geom_smooth( method=lm,color = "red")
#Final Output 
ggplot(data = exercise, aes(y=phys_score, x=sex , fill = sex)) + geom_boxplot()+ labs(x= "Gender", y= "Scale of physical score", title = "Physical Score Between Gender")

# Relationship between mental health score and exp score 
ggplot(data = exercise, aes(x=MH_score, y = exp_mark, colour=day)) + geom_point(size=2)+ geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=MH_score, y = exp_mark, colour=day)) + geom_point(size=2)+ facet_grid(~sex) + geom_smooth( method=lm,color = "red")
ggplot(data = exercise, aes(x=MH_score, y = exp_mark, colour=day)) + geom_point(size=2)+ facet_grid(~age) + geom_smooth( method=lm,color = "red")
#Final Output 
ggplot(data = exercise, aes(y=MH_score, x=sex , fill = sex)) + geom_boxplot()+ labs(x= "Gender", y= "Mental Health score", title = "Mental Health Score Between Gender")

