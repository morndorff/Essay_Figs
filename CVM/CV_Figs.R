# Cvm One Samp
rm(list=ls())
set.seed(5)

TEXTSIZE <- 18

# One Sample CVM-Test

x <- sort(rnorm(10,0,1))
lensamp <- length(x)
y1 <- seq(1/lensamp,1,length.out=lensamp)
i <- seq_along(x)

# Data
x <- x
y <- (2*i-1)/(2*lensamp)
df_data <- data.frame(x=x,y=y)

# Actual Distribution
x_val <- seq(min(x)- (1/length(x))*diff(range(x)), max(x) + (1/length(x)*diff(range(x))), length.out=1000)
y_val <- pnorm(x_val)
df_dist <- data.frame(x=x_val,y=y_val)

# Dashed Lines to Points
xstart <- x
xend <- x
yend <- pnorm(x)
ystart <- y
df_dashed <- data.frame(x=xstart, y=ystart, xend=xend, y=yend)

# Squared Lengths
i <- seq_along(x)
sign_stor <- sign((2*i-1)/(2*lensamp)-pnorm(x))
bigdist <- ((2*i-1)/(2*lensamp)-pnorm(x))^2 * sign_stor

x2start <- x
x2end <- x
y2start <- pnorm(x)
y2end <- pnorm(x) + bigdist
df_squared <- data.frame(x=x2start, y=y2start, xend=x2end, yend=y2end)

plot1 <- ggplot(df_data) +
  #geom_point(aes(x=x, y=y, xend=xend, yend=yend), 
  #             size=.8, colour="blue") +

  geom_line(data=df_dist, aes(x=x,y=y)) +
  geom_segment(data=df_dashed, aes(x=x,y=y,xend=xend,yend=yend),
                colour="red", linetype="dashed") +
  geom_segment(data=df_squared, aes(x=x,y=y,xend=xend,yend=yend),
               colour="blue", size=1.2) +
  geom_hline(yintercept=c(0,1), linetype="dashed", alpha=.5) +
  geom_point(data=df_data, aes(x=x, y=y), 
             size=5, colour="blue", shape="circle") + 
  # ggtitle("One Sample Kolmogorov-Snirnov Test Statistic") +
  ylab("Cumulative Probability") +
  xlab("Data") + 
  ylim(c(0,1)) +
  xlim(min(x)- (1/length(x))*diff(range(x)),max(x)+ (1/length(x)*diff(range(x)))) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.x = element_text(size=TEXTSIZE), 
        axis.title.y = element_text(size=TEXTSIZE), 
        axis.text.x  = element_text(size=TEXTSIZE),
        axis.text.y  = element_text(size=TEXTSIZE))
plot1
ggsave("cvm_one_samp.pdf")
