# Two Sample CVM Plot
# 7.64 x 7.14
rm(list=ls())
# PARAMETERS
TEXTSIZE <- 18
seed <- 25
# Two Sample Plot
set.seed(seed)
x <- sort(rnorm(5))
y <- sort(rnorm(7))
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


# Figuring out distances
set.seed(seed)
x <- sort(rnorm(5))
y <- sort(rnorm(7))
z1 <- c(x,y)
z2 <- c(seq(1/length(x),1,length.out=length(x)),
        seq(1/length(y),1,length.out=length(y)))
xstart <- z1
xend <- z1
ystart <-  z2
yend <- c(f_y(x), f_x(y))
df_dashed <- data.frame(x=xstart, y=ystart, xend=xend, yend=yend)

# Adding Lengths
len <- (ystart - yend)^2
sign2 <- sign(c(z2[1:length(x)]-f_y(x),
                z2[(length(x)+1) : length(z2)]-f_x(y)))
yend2 <- ystart - len*sign2
df_bold <- data.frame(x=xstart, y=ystart, xend=xend, yend=yend2)


plot2 <- ggplot(df_data_x) +
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend), 
               size=.8, colour="blue", alpha=.5) +
  geom_segment(data=df_data_y, aes(x=x, y=y, xend=xend, yend=yend), 
               size=.8, colour="red", alpha=.5) +
  geom_point(data=df_data_x[-1,], aes(x=x, y=y), 
             size=3, colour="blue") + 
  geom_point(data=df_data_y[-1,], aes(x=x, y=y), 
             size=3, colour="red") +
  geom_segment(data=df_dashed, aes(x=x,y=y,xend=xend,yend=yend), 
               colour="black", linetype="dashed") +
  geom_segment(data=df_bold, aes(x=x,y=y,xend=xend,yend=yend), 
               colour="black", size=1.5) +  
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
ggsave("cvm_two_samp.pdf")