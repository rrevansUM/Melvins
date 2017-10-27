data <- c(1,2,3,4)
mean = data[1]
RSS = 0

for(i in 2:4){
	RSS = RSS + ((i-1)/i)*(data[i]-mean)^2
	mean = mean + (data[i]-mean)/i
}

RSS
mean
stdDev = sqrt(RSS/(length(data)-1))