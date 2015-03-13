# KS Test Figures

# KS-Test Figures
# Generating the figures for the K-S Test
# rm(list=ls())
set.seed(5)
library(ggplot2)
TEXTSIZE <- 18

# One Sample KS-Test

set.seed(5)
x <- sort(rnorm(10,0,1))
f_x <- ecdf(x)
lensamp <- length(x)
y1 <- seq(1/lensamp,1,length.out=lensamp)

# Data
xbegin <- min(x) - diff(range(x))/length(x)
xend <- append(x, max(x) + diff(range(x))/length(x))
x <- append(xbegin, x)
y1 <- append(0, y1)
df_data <- data.frame(x=x,y=y1, xend=xend, yend=y1)

# Actual Distribution
x_val <- seq(min(x), max(xend), length.out=1000)
y_val <- pnorm(x_val)
df_dist <- data.frame(x=x_val,y=y_val)

# Creating Maximum Dashed LIne
x_max <- which.max(abs(f_x(x_val)-y_val))
x0 <- x_val[x_max]
y0 <- y_val[x_max]
y1 <- f_x(x0)
df_max <- data.frame(x=x0, y=y0, xend=x0, yend=y1)




plot1 <- ggplot(df_data) +
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend), 
               size=.8, colour="blue") +
  geom_point(data=df_data[-1,], aes(x=x, y=y), 
             size=3, colour="blue") + 
  geom_line(data=df_dist, aes(x=x,y=y)) +
  geom_segment(data=df_max, aes(x=x,y=y,xend=xend,yend=yend), 
               colour="red", linetype="dashed") +
  geom_hline(yintercept=c(0,1), linetype="dashed", alpha=.5) +
  # ggtitle("One Sample Kolmogorov-Snirnov Test Statistic") +
  ylab("Cumulative Probability") +
  xlab("Data") + 
  ylim(c(0,1)) +
  xlim(min(x),max(xend)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.x = element_text(size=TEXTSIZE), 
        axis.title.y = element_text(size=TEXTSIZE), 
        axis.text.x  = element_text(size=TEXTSIZE),
        axis.text.y  = element_text(size=TEXTSIZE))
plot1
ggsave("ks_one_samp.pdf")
# Learning how to export plots correctly


# Two Sample Plot
set.seed(5)
x <- sort(rnorm(10))
y <- sort(rnorm(12))
f_x <- ecdf(x)
f_y <- ecdf(y)
z <- sort(c(x,y))

lenx <- length(x)
leny <- length(y)
y1_x <- seq(1/lenx,1,length.out=lenx)
y1_y <- seq(1/leny,1,length.out=leny)

begin <- min(z) - diff(range(z))/length(z)
end <- max(z) + diff(range(z)/length(z))

# X Data
xend <- append(x, end)
x <- append(begin, x)
y1_x <- append(0, y1_x)
df_data_x <- data.frame(x=x,y=y1_x, xend=xend, yend=y1_x)

# Y Data
yend <- append(y, end)
y <- append(begin, y)
y1_y <- append(0, y1_y)
df_data_y <- data.frame(x=y,y=y1_y, xend=yend, yend=y1_y)

# Where Put Max?
t <- seq(min(z), max(z), length.out=length(x))
x0 <- t[which(abs(f_y(t) - f_x(t)) == max(abs(f_x(t) - f_y(t))))] 
y0 <- f_x(x0)
y1 <- f_y(x0)
df_data_max <- data.frame(x=x0, y=y0, xend=x0, yend=y1)


plot2 <- ggplot(df_data_x) +
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend), 
               size=.8, colour="blue", alpha=.5) +
  geom_segment(data=df_data_y, aes(x=x, y=y, xend=xend, yend=yend), 
               size=.8, colour="red", alpha=.5) +
  geom_point(data=df_data_x[-1,], aes(x=x, y=y), 
             size=3, colour="blue") + 
  geom_point(data=df_data_y[-1,], aes(x=x, y=y), 
             size=3, colour="red") +
  geom_segment(data=df_data_max, aes(x=x,y=y,xend=xend,yend=yend), 
               colour="black", linetype="dashed") +
  geom_hline(yintercept=c(0,1), linetype="dashed", alpha=.5) +
  # ggtitle("Two Sample Kolmogorov-Snirnov Test Statistic") +
  ylab("Cumulative Probability") +
  xlab("Data") + 
  ylim(c(0,1)) +
  xlim(min(x),max(xend)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.x = element_text(size=TEXTSIZE), 
        axis.title.y = element_text(size=TEXTSIZE), 
        axis.text.x  = element_text(size=TEXTSIZE),
        axis.text.y  = element_text(size=TEXTSIZE))
  
plot2
ggsave("ks_two_samp.pdf")






# 
# 
# x <- sort(rnorm(10,0,1))
# lensamp <- length(x)
# y1 <- seq(1/lensamp,1,length.out=lensamp)
# #df <- data.frame(x =x , y=y1)
# 
# # # qplot(rnorm(15), stat = "ecdf", geom = "step")
# # plot1 <- ggplot(df, aes(x=x, y=y)) +
# #   geom_step(size=1) +
# #   geom_point(size=2.5) +
# #   labs(shape = "ECDFs", color = "ECDFs") +
# #   ylab("Cumulative Probability") +
# #   xlab("") + 
# #   ylim(c(0,1)) +
# #   #scale_color_discrete(name="ECDFs") +
# #   #scale_colour_manual(values=cbPalette1) +
# #   theme_classic() +
# #   theme(axis.title.x = element_text(size=15), 
# #         axis.title.y = element_text(size=15), 
# #         axis.text.x  = element_text(size=13),
# #         axis.text.y  = element_text(size=13))
# # plot1
# 
# f_x <- ecdf(x)
# y <- seq(-3,3, length.out=500)
# y1 <- pnorm(y,0,1)
# x_ind <- which.max(abs(f_x(y)-y1))
# x0 <- y[x_ind]
# plot(f_x, main="One Sample K-S Test Statistic", ylab="Fn(x), Empirical CDF", xlab="Data", col="blue")
# lines(y,y1)
# segments(x0,f_x(x0),x0,y1[x_ind], col="red", lwd=2, lty="dotted")
# dev.off()