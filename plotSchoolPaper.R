data = read.csv('/home/kuangjun/course/统计推断/国家优秀博士论文信息.csv',header=TRUE)
#从数据集中选取学校代码和发表论文数两列，拼成一个新的列表
school_subject_paper_number = data.frame(c(data['博士所在学校代码'] , data['发表论文数1'],data['一级学科码']))
colnames(school_subject_paper_number) = c('school','paper','subject')
#得到每个学校优秀博士发表论文数的均值
mean_school_paper = aggregate(school_subject_paper_number['paper'],by=list(school=factor(school_subject_paper_number$school)),mean)
colnames(mean_school_paper) = c('school','paper')
library(ggplot2)
library(gcookbook)
ggplot(mean_school_paper, aes(x=factor(school), y=paper)) + geom_bar(stat="identity")+labs(title = "Average number of paper for outstanding phd in each school")+pdf('school_paper_hist.pdf')+theme(axis.text.x = element_text(size=3,angle=90))

#论文数描述性统计量
print("论文数描述性统计量")
summary(school_subject_paper_number)
print("论文数方差")
var(school_subject_paper_number['paper'])
#学校数量
print("学校数量")
print(lengths(unique(school_subject_paper_number['school'])))
#学科数量
print("学科数量")
print(lengths(unique(school_subject_paper_number['subject'])))
#subject_paper_number = c(data['一级学科码'],data['发表论文数1'])
#得到每个学科的优秀博士发表论文数的均值
mean_subject_paper = aggregate(school_subject_paper_number['paper'],by=list(subject = factor(school_subject_paper_number$subject)),mean)
colnames(mean_subject_paper) = c('subject','paper')
ggplot(mean_subject_paper,aes(x=factor(subject),y=paper)) + geom_bar(stat="identity")+labs(title= "Average number of paper for outstanding phd in each subject")+pdf('subject_paper_hist.pdf')+theme(axis.text.x = element_text(size=3,angle=90))

#学校平均论文数的最大值、最小值、平均数和方差
v_mean_school_paper = as.vector(unlist(mean_school_paper['paper']))
print("各学校优秀博士平均论文数的均值")
mean(v_mean_school_paper)
print("各学校优秀博士平均论文数的中位数")
median(v_mean_school_paper)
print("各学校优秀博士平均论文数的方差")
var(v_mean_school_paper)
#学科平均论文数的最大值、最小值、平均数和方差
v_mean_subject_paper = as.vector(unlist(mean_subject_paper['paper']))
print("各学科优秀博士平均论文数的均值")
mean(v_mean_subject_paper)
print("各学科优秀博士平均论文数的中位数")
median(v_mean_subject_paper)
print("各学科优秀博士平均论文数的方差")
var(v_mean_subject_paper)




#单因素方差分析
#正态性检验
#v_mean_school_paper = as.vector(unlist(mean_school_paper['paper']))
#shapiro.test(v_mean_school_paper)
#v_mean_subject_paper = as.vector(unlist(mean_subject_paper['paper']))
#shapiro.test(v_mean_subject_paper)



#方差齐性检验
library(car)
print('方差齐性检验(school)')
leveneTest(school_subject_paper_number$paper,factor(school_subject_paper_number$school))
print('方差齐性检验(subject)')
leveneTest(school_subject_paper_number$paper,factor(school_subject_paper_number$subject))

library(multcomp)

print('差异显著性检验(School)')
school_paper_result=aov(paper~factor(school), data=school_subject_paper_number)
summary(school_paper_result)
print('差异显著性检验(Subject)')
subject_paper_result=aov(paper~factor(subject) , data = school_subject_paper_number)
summary(subject_paper_result)
#school_subject_result = aov(paper~factor(school)*factor(subject),data = school_subject_paper_number)
#summary(school_subject_results

#正态性检验
v_paper_number=as.vector(unlist(school_subject_paper_number['paper']))


shapiro.test(v_paper_number)


#kruskal 检验
print('kruskal–wallis检验(School)')
kruskal.test(paper~factor(school),school_subject_paper_number)
print('kruskal–wallis检验(Subject)')
kruskal.test(paper~factor(subject),school_subject_paper_number)


#发表论文数和影响因子的相关性分析
paper_number = school_subject_paper_number['paper']
sci_number=data['SCI数']
if_number = data['影响因子']
v_paper_number = as.vector(unlist(paper_number))
v_if_number = as.vector(unlist(if_number))
v_sci_number = as.vector(unlist(sci_number))
setwd("~/")
jpeg(file="myplot.jpeg")
plot(v_sci_number,v_if_number)
model = lm(v_if_number~v_sci_number)
abline(lm(v_if_number~v_sci_number))
dev.off()
summary(model)
#jpeg(file="plotAnalyse.jpeg")
#par(mfrow=c(2,2))
#plot(model)
#dev.off()
residual = as.vector(unlist(residuals(model)))
shapiro.test(residual)
cor.test(v_sci_number,v_if_number)
