#loading libraries

library(car)
library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

##########################################################
#loading data
Base.Cars<-read.csv(file='CarPrice_Assignment.csv',stringsAsFactors = F,check.names = F)

#making a coppy for all the processing
Data.Cars<-Base.Cars

summary(Data.Cars)
str(Data.Cars)
nrow(Data.Cars)#205 rows

length(unique(Data.Cars$car_ID))#205 unique id's No duplication

sum(is.na(Data.Cars))
#no nulls

#univariant analysis
QuartileNHist <- function(data) 
{
  print(quantile(data,seq(0, 1, 0.01)))
  print(ggplot(data.frame(data),aes(x=data))+geom_histogram() + scale_x_continuous(breaks = round(seq(min(data), max(data), by = (max(data)-min(data))/30 ),1)))
  
}


##########################################################
mutatecol<- function(colname,partition)
{
  clause<- paste(" ",colname ," = case_when( ")
  for (i in 1:length(partition))  
  { 
    if (i==1)
    { clause<-paste(clause , " ( ",colname ," <= ", partition[i]," )~ ",i," , ")
    
    } else if (i>1 
               # & i!=length(partition)
    )
    {
      clause<-paste(clause, " ( ", colname ," > ", partition[i-1]," & ",colname ," <= ",partition[i]," ) ~ ",i," , ")
    } 
    
    
    
    
    
  }
  clause<-paste(clause , " ( ", colname ," > ", partition[length(partition)]," )~ ",length(partition)+1," ,T~",length(partition)+2," ) ")
  # clause<-paste( " lazyeval::interp( ", clause , ")") 
  print(clause)  
  #Data.Cars3<-mutate_(Data.Cars3,clause)
}
##############################################
#carlength
QuartileNHist(Data.Cars$carlength)
#their is clustering in the middle (165-177) and smaller peaks both side; 
#candidate for binning
partition<-c(152.3,160,165.3,177.66,183.544,184.760,192.420,196.360,198)
mutatecol('carlength',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars, carlength  = case_when(   (  carlength  <=  152.3  )~  1  ,   (  carlength  >  152.3  &  carlength  <=  160  ) ~  2  ,   (  carlength  >  160  &  carlength  <=  165.3  ) ~  3  ,   (  carlength  >  165.3  &  carlength  <=  177.66  ) ~  4  ,   (  carlength  >  177.66  &  carlength  <=  183.544  ) ~  5  ,   (  carlength  >  183.544  &  carlength  <=  184.76  ) ~  6  ,   (  carlength  >  184.76  &  carlength  <=  192.42  ) ~  7  ,   (  carlength  >  192.42  &  carlength  <=  196.36  ) ~  8  ,   (  carlength  >  196.36  &  carlength  <=  198  ) ~  9  ,   (  carlength  >  198  )~  10  ,T~ 11  ))



#wheelbase
QuartileNHist(Data.Cars$wheelbase)
#densely populated between 93-100
#then smaller spikes of reducing height as we move to right, 
#same is refelcted by bigger jupns in the %tile values to wards the bigger number
#another candidate for binning
partition<-c(87.7,92.3,97.3,98.4,99.2,102.4,104.66,105.8,107.324,110.6,115.544)
mutatecol('wheelbase',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars,wheelbase  = case_when(   (  wheelbase  <=  87.7  )~  1  ,   (  wheelbase  >  87.7  &  wheelbase  <=  92.3  ) ~  2  ,   (  wheelbase  >  92.3  &  wheelbase  <=  97.3  ) ~  3  ,   (  wheelbase  >  97.3  &  wheelbase  <=  98.4  ) ~  4  ,   (  wheelbase  >  98.4  &  wheelbase  <=  99.2  ) ~  5  ,   (  wheelbase  >  99.2  &  wheelbase  <=  102.4  ) ~  6  ,   (  wheelbase  >  102.4  &  wheelbase  <=  104.66  ) ~  7  ,   (  wheelbase  >  104.66  &  wheelbase  <=  105.8  ) ~  8  ,   (  wheelbase  >  105.8  &  wheelbase  <=  107.324  ) ~  9  ,   (  wheelbase  >  107.324  &  wheelbase  <=  110.6  ) ~  10  ,   (  wheelbase  >  110.6  &  wheelbase  <=  115.544  ) ~  11  ,   (  wheelbase  >  115.544  )~  12  ,T~ 13  ))

#carwidth
QuartileNHist(Data.Cars$carwidth)
#before 63 their are outliner
#66.5 to 67 have a big block
# there are two more groups to the right
# again candidate for binning
partition<-c(61.1,61.9,63.5,64.6,65.5,66.5,67.3,68.4,69.6,71.1,71.9)
mutatecol('carwidth',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars,carwidth  = case_when(   (  carwidth  <=  61.1  )~  1  ,   (  carwidth  >  61.1  &  carwidth  <=  61.9  ) ~  2  ,   (  carwidth  >  61.9  &  carwidth  <=  63.5  ) ~  3  ,   (  carwidth  >  63.5  &  carwidth  <=  64.6  ) ~  4  ,   (  carwidth  >  64.6  &  carwidth  <=  65.5  ) ~  5  ,   (  carwidth  >  65.5  &  carwidth  <=  66.5  ) ~  6  ,   (  carwidth  >  66.5  &  carwidth  <=  67.3  ) ~  7  ,   (  carwidth  >  67.3  &  carwidth  <=  68.4  ) ~  8  ,   (  carwidth  >  68.4  &  carwidth  <=  69.6  ) ~  9  ,   (  carwidth  >  69.6  &  carwidth  <=  71.1  ) ~  10  ,   (  carwidth  >  71.1  &  carwidth  <=  71.9  ) ~  11  ,   (  carwidth  >  71.9  )~  12  ,T~ 13  ))

#carheight
QuartileNHist(Data.Cars$carheight)
#the data is distributed with some segmentation towards the higher values
partition<-c(49.416,50.952,51.6,53.7,57,58,59.6)
mutatecol('carheight',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars,carheight  = case_when(   (  carheight  <=  49.416  )~  1  ,   (  carheight  >  49.416  &  carheight  <=  50.952  ) ~  2  ,   (  carheight  >  50.952  &  carheight  <=  51.6  ) ~  3  ,   (  carheight  >  51.6  &  carheight  <=  53.7  ) ~  4  ,   (  carheight  >  53.7  &  carheight  <=  57  ) ~  5  ,   (  carheight  >  57  &  carheight  <=  58  ) ~  6  ,   (  carheight  >  58  &  carheight  <=  59.6  ) ~  7  ,   (  carheight  >  59.6  )~  8  ,T~ 9  ))
                  
#curbweight
QuartileNHist(Data.Cars$curbweight)
#their are ouliners are both end of data 
partition<-c(1831.7,2433,3120,3471.80,3737)
mutatecol('curbweight',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars, curbweight  = case_when(   (  curbweight  <=  1831.7  )~  1  ,   (  curbweight  >  1831.7  &  curbweight  <=  2433  ) ~  2  ,   (  curbweight  >  2433  &  curbweight  <=  3120  ) ~  3  ,   (  curbweight  >  3120  &  curbweight  <=  3471.8  ) ~  4  ,   (  curbweight  >  3471.8  &  curbweight  <=  3737  ) ~  5  ,   (  curbweight  >  3737  )~  6  ,T~ 7  ))


#enginesize
QuartileNHist(Data.Cars$enginesize)
#their is a longish tale on the higher side 
#87.5, 122 is where  highest concentartion is
partition<-c(87.5,122.8,140.5,158.2,181,194,220)
mutatecol('enginesize',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars,enginesize  = case_when(   (  enginesize  <=  87.5  )~  1  ,   (  enginesize  >  87.5  &  enginesize  <=  122.8  ) ~  2  ,   (  enginesize  >  122.8  &  enginesize  <=  140.5  ) ~  3  ,   (  enginesize  >  140.5  &  enginesize  <=  158.2  ) ~  4  ,   (  enginesize  >  158.2  &  enginesize  <=  181  ) ~  5  ,   (  enginesize  >  181  &  enginesize  <=  194  ) ~  6  ,   (  enginesize  >  194  &  enginesize  <=  220  ) ~  7  ,   (  enginesize  >  220  )~  8  ,T~ 9  ))
                  
                  
#boreratio
QuartileNHist(Data.Cars$boreratio)
# ouliner on bith side
partition<-c(2.8,2.95,3.05,3.2,3.26,3.35,3.5,3.65,3.85)
mutatecol('boreratio',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars,boreratio  = case_when(   (  boreratio  <=  2.8  )~  1  ,   (  boreratio  >  2.8  &  boreratio  <=  2.95  ) ~  2  ,   (  boreratio  >  2.95  &  boreratio  <=  3.05  ) ~  3  ,   (  boreratio  >  3.05  &  boreratio  <=  3.2  ) ~  4  ,   (  boreratio  >  3.2  &  boreratio  <=  3.26  ) ~  5  ,   (  boreratio  >  3.26  &  boreratio  <=  3.35  ) ~  6  ,   (  boreratio  >  3.35  &  boreratio  <=  3.5  ) ~  7  ,   (  boreratio  >  3.5  &  boreratio  <=  3.65  ) ~  8  ,   (  boreratio  >  3.65  &  boreratio  <=  3.85  ) ~  9  ,   (  boreratio  >  3.85  )~  10  ,T~ 11  ))
                  
#stroke
QuartileNHist(Data.Cars$stroke)
# 3-3.65 has concentration 
#  their are batched around
partition<-c(2.2,2.55,2.65,3,3.15,3.35,3.65,4)
mutatecol('stroke',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars, stroke  = case_when(   (  stroke  <=  2.2  )~  1  ,   (  stroke  >  2.2  &  stroke  <=  2.55  ) ~  2  ,   (  stroke  >  2.55  &  stroke  <=  2.65  ) ~  3  ,   (  stroke  >  2.65  &  stroke  <=  3  ) ~  4  ,   (  stroke  >  3  &  stroke  <=  3.15  ) ~  5  ,   (  stroke  >  3.15  &  stroke  <=  3.35  ) ~  6  ,   (  stroke  >  3.35  &  stroke  <=  3.65  ) ~  7  ,   (  stroke  >  3.65  &  stroke  <=  4  ) ~  8  ,   (  stroke  >  4  )~  9  ,T~ 10  ))


#compressionratio
QuartileNHist(Data.Cars$compressionratio)
#data is clustered between (7-10.2) and  (20-23.*)
partition<-c(7.5,8.1,8.6,9.7,12.9,21.65,22.42)
mutatecol('compressionratio',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars,compressionratio  = case_when(   (  compressionratio  <=  7.5  )~  1  ,   (  compressionratio  >  7.5  &  compressionratio  <=  8.1  ) ~  2  ,   (  compressionratio  >  8.1  &  compressionratio  <=  8.6  ) ~  3  ,   (  compressionratio  >  8.6  &  compressionratio  <=  9.7  ) ~  4  ,   (  compressionratio  >  9.7  &  compressionratio  <=  12.9  ) ~  5  ,   (  compressionratio  >  12.9  &  compressionratio  <=  21.65  ) ~  6  ,   (  compressionratio  >  21.65  &  compressionratio  <=  22.42  ) ~  7  ,   (  compressionratio  >  22.42  )~  8  ,T~ 9  )  )      



#horsepower
QuartileNHist(Data.Cars$horsepower)
#60-128 is dencely concentrated
partition<-c(52.12,56,62,65.12,76.48,100.2,107.76,120.04,128,144,160,188,210)
mutatecol('horsepower',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars, horsepower  = case_when(   (  horsepower  <=  52.12  )~  1  ,   (  horsepower  >  52.12  &  horsepower  <=  56  ) ~  2  ,   (  horsepower  >  56  &  horsepower  <=  62  ) ~  3  ,   (  horsepower  >  62  &  horsepower  <=  65.12  ) ~  4  ,   (  horsepower  >  65.12  &  horsepower  <=  76.48  ) ~  5  ,   (  horsepower  >  76.48  &  horsepower  <=  100.2  ) ~  6  ,   (  horsepower  >  100.2  &  horsepower  <=  107.76  ) ~  7  ,   (  horsepower  >  107.76  &  horsepower  <=  120.04  ) ~  8  ,   (  horsepower  >  120.04  &  horsepower  <=  128  ) ~  9  ,   (  horsepower  >  128  &  horsepower  <=  144  ) ~  10  ,   (  horsepower  >  144  &  horsepower  <=  160  ) ~  11  ,   (  horsepower  >  160  &  horsepower  <=  188  ) ~  12  ,   (  horsepower  >  188  &  horsepower  <=  210  ) ~  13  ,   (  horsepower  >  210  )~  14  ,T~ 15  ))
                  
                  
#peakrpm
QuartileNHist(Data.Cars$peakrpm)
#data is segmneted into 4 segment with one outliner on the right side
partition<-c(4558,5130,5650,6191)
mutatecol('peakrpm',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars,  peakrpm  = case_when(   (  peakrpm  <=  4558  )~  1  ,   (  peakrpm  >  4558  &  peakrpm  <=  5130  ) ~  2  ,   (  peakrpm  >  5130  &  peakrpm  <=  5650  ) ~  3  ,   (  peakrpm  >  5650  &  peakrpm  <=  6191  ) ~  4  ,   (  peakrpm  >  6191  )~  5  ,T~ 6  ) )


#citympg
QuartileNHist(Data.Cars$citympg)
#few outliner on teh right side with
#Big spikes in the data pointing to concentartion at specific intervals
partition<-c(15.4,16.6,17.8,19,23.2,24.4,30.4,31.6,36.4,40)
mutatecol('citympg',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars,citympg  = case_when(   (  citympg  <=  15.4  )~  1  ,   (  citympg  >  15.4  &  citympg  <=  16.6  ) ~  2  ,   (  citympg  >  16.6  &  citympg  <=  17.8  ) ~  3  ,   (  citympg  >  17.8  &  citympg  <=  19  ) ~  4  ,   (  citympg  >  19  &  citympg  <=  23.2  ) ~  5  ,   (  citympg  >  23.2  &  citympg  <=  24.4  ) ~  6  ,   (  citympg  >  24.4  &  citympg  <=  30.4  ) ~  7  ,   (  citympg  >  30.4  &  citympg  <=  31.6  ) ~  8  ,   (  citympg  >  31.6  &  citympg  <=  36.4  ) ~  9  ,   (  citympg  >  36.4  &  citympg  <=  40  ) ~  10  ,   (  citympg  >  40  )~  11  ,T~ 12  ))


#highwaympg
QuartileNHist(Data.Cars$highwaympg)

partition<-c(21.1,25.5,26.7,34.3,36.04,38.64,43.9,48)
mutatecol('highwaympg',partition)
#segmenting the data set
Data.Cars<-mutate(Data.Cars, highwaympg  = case_when(   (  highwaympg  <=  21.1  )~  1  ,   (  highwaympg  >  21.1  &  highwaympg  <=  25.5  ) ~  2  ,   (  highwaympg  >  25.5  &  highwaympg  <=  26.7  ) ~  3  ,   (  highwaympg  >  26.7  &  highwaympg  <=  34.3  ) ~  4  ,   (  highwaympg  >  34.3  &  highwaympg  <=  36.04  ) ~  5  ,   (  highwaympg  >  36.04  &  highwaympg  <=  38.64  ) ~  6  ,   (  highwaympg  >  38.64  &  highwaympg  <=  43.9  ) ~  7  ,   (  highwaympg  >  43.9  &  highwaympg  <=  48  ) ~  8  ,   (  highwaympg  >  48  )~  9  ,T~ 10  ))


#Since we segmented all the continous variable;
#the need for creating derived varailbe lile mpg/horsepower, mpg/curbweight etc are negated  

#categorical columns handling

###########################################################
Cate.Var.Conv.2level<- function(colname)
{
  col.number<-which( colnames(Data.Cars)==colname )
  uni.values<-length(unique(Data.Cars[,col.number]))
  Data.Cars[,col.number]<-factor(Data.Cars[,col.number])
  print(str(Data.Cars[,col.number]))
  levels(Data.Cars[,col.number])<-c(0:(uni.values-1))
  print(str(Data.Cars[,col.number]))
  Data.Cars[,col.number]<- as.numeric(levels(Data.Cars[,col.number]))[Data.Cars[,col.number]]
  Data.Cars[,col.number]
  Data.Cars <<- Data.Cars  
}


####################################################################
summary(Data.Cars)
#getting manufacturar name
Data.Cars$Manufacturar<-gsub( " .*$", "", Data.Cars$CarName )
unique(Data.Cars$Manufacturar)
#cleaning up data in consitancey in manufacturar name
#Nissan-nissan;
Data.Cars$Manufacturar<-gsub( "nissan", "Nissan", Data.Cars$Manufacturar )
#maxda-mazda;
Data.Cars$Manufacturar<-gsub( "maxda", "mazda", Data.Cars$Manufacturar )
#vokswagen-volkswagen-vw;
Data.Cars$Manufacturar<-gsub( "vokswagen", "volkswagen", Data.Cars$Manufacturar )
Data.Cars$Manufacturar<-gsub( "vw", "volkswagen", Data.Cars$Manufacturar )
#porcshce-porsche;
Data.Cars$Manufacturar<-gsub( "porcshce", "porsche", Data.Cars$Manufacturar )
#toyouta-toyota
Data.Cars$Manufacturar<-gsub( "toyouta", "toyota", Data.Cars$Manufacturar )
unique(Data.Cars$Manufacturar)

#unique value in each factor field
rapply(Data.Cars[, sapply(Data.Cars, class) == 'character'],function(x)length(unique(x)))
# fueltype,aspiration,doornumber,enginelocation have only 2 levels
# carbody,drivewheel,enginetype,cylindernumber,fuelsystem,Manufacturar have multipel levels

#handling 2 level columns
Cate.Var.Conv.2level("fueltype")
#"diesel","gas"=>0,1
Cate.Var.Conv.2level("aspiration")
#"std","turbo=>0,1
Cate.Var.Conv.2level("doornumber")
#"four","two"=>0,1
Cate.Var.Conv.2level("enginelocation")
#"front","rear"=>0,1

#factor with more tahn 2 level
factor.var<-model.matrix(~carbody+drivewheel+enginetype+cylindernumber+fuelsystem+Manufacturar,data=Data.Cars)
factor.var<-factor.var[,-1]

#dropping the original categorical colum
drops <- c("carbody","drivewheel","enginetype","cylindernumber","fuelsystem","Manufacturar")
Data.Cars<-Data.Cars[ , !(names(Data.Cars) %in% drops)]
#combining the modified categorical columns set to the data
Data.Cars<-cbind(Data.Cars,factor.var)
summary(Data.Cars)

#dropping the id(unique number can not contribute to the model) and name(manufacturar details extracted) columns which are not required for preducition
drops <-c("car_ID","CarName")
Data.Cars<-Data.Cars[ , !(names(Data.Cars) %in% drops)]
summary(Data.Cars)

#####################################
# although teh data set is small, we will still spilt it for validation purpose
# we will run bootstart on teh complete data set and the test data set to validate our finding

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(Data.Cars), 0.7*nrow(Data.Cars))
train = Data.Cars[trainindices,]
test = Data.Cars[-trainindices,]

#chain co lienarity between variables only for continuous variable ;
#the categorial data is not interpreted by choice
cor_train<-cor(train)
write.csv(cor_train,'cor_train.csv')
View(cor_train)
#symboling:doornumber=0.66;peakrpm:fueltype=0.49;carbodyhatchback:doornumber=.61
#following variabel are highly co-related to each other as they structural property of car along with engine
#wheelbase;carlength;carwidth;carheight;curbweight;enginesize;boreratio;horsepower;drivewheelrwd



#######################


model_1.1 <- lm(formula = price~. , data = train)
summary(model_1.1)
#Adjusted R-squared:  0.9736 
#The vraible with  coeffients,p-value etc as NA are statistically insignificant
#they are listed below
#Manufacturarsubaru,Manufacturarporsche,Manufacturarpeugeot,Manufacturarchevrolet,
#fuelsystemspfi,fuelsystemidi,fuelsystem4bbl,cylindernumbertwelve,cylindernumbertwo

#using step aic to remove the multi-linear variables
#stepaic
step <- stepAIC(model_1.1, direction="both")
step

##model 1.2 given by  step aic
model_1.2 <- lm(price ~ symboling + fueltype + aspiration + enginelocation + 
  wheelbase + carlength + carwidth + carheight + curbweight + 
  enginesize + compressionratio + horsepower + citympg + carbodyhardtop + 
  carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
  enginetypeohcv + cylindernumberfive + cylindernumberfour + 
  cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
  fuelsystemmpfi + Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
  Manufacturarisuzu + Manufacturarmazda + Manufacturarmitsubishi + 
  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)

summary(model_1.2)
#Adjusted R-squared:  0.9758 


#checking VIF
cor.var<-as.matrix(vif(model_1.2))
cor.var
#their are lots of variable with high vif we will iterate over them oen by one 
#####################################################
#funtion forcalulation next iterations
next.iteration<- function(imodel,i)
{
  cor.var<-as.matrix(vif(imodel))
  cor.var <- data.frame(row.names(cor.var), cor.var, row.names = NULL)
  n<-length(cor.var[,2])
  print('VIF')
  print(cor.var[which(cor.var$cor.var==sort(cor.var[,2],partial=n-i)[n-i]),])
  
  test.var<-as.character(cor.var[which(cor.var$cor.var==sort(cor.var[,2],partial=n-i)[n-i]),1])
  
  p.var<-as.matrix(summary(imodel)$coefficients[,4])
  p.var <- data.frame(row.names(p.var), p.var, row.names = NULL)
  print(paste('P-value for ',test.var,':-'))
  print(p.var[which(p.var$row.names.p.var.==test.var),])
  
}
########################################
next.iteration(model_1.2,0)
# Variable                VIF       P-value 
# cylindernumberfour    25.05008    0.01501956
#althogh vif is high but p-valis is significant, we will vist this varibale agian if need be, 
#for now keeping it.
###############################################
next.iteration(model_1.2,1)
# Variable        VIF       P-value 
# horsepower    20.25553    0.1067165
#since vif and p value both are high we will remove the varabel and buidl the next model
#earlier we saw hrosepwoer in teh set of co related continous varaible also
############################################
model_1.3 <- lm(price ~ symboling + fueltype + aspiration + enginelocation + 
                  wheelbase + carlength + carwidth + carheight + curbweight + 
                  enginesize + compressionratio  + citympg + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                  fuelsystemmpfi + Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarisuzu + Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)

summary(model_1.3)
# R-squared:  0.9824,	Adjusted R-squared:  0.9754 
#minimal imapct of adjusted r-squared so we are good
###########################################
#lets continue next iteration with model_1.3
next.iteration(model_1.3,0)   
# Variable                VIF       P-value 
# cylindernumberfour    24.40505    0.007067993
#cylindernumberfour become more significant in this model
next.iteration(model_1.3,1)   
# Variable          VIF       P-value 
# carbodysedan    18.91184    0.02846494
#stil significant p value lets try a level lower
next.iteration(model_1.3,2)   
# Variable          VIF       P-value 
# wheelbase    16.49464    0.0006192239
#p is ver significant
next.iteration(model_1.3,3)   
# Variable          VIF       P-value 
# carbodyhatchback  16.20613  0.02052965

next.iteration(model_1.3,4)# very low p-value   
# Variable      VIF       P-value 
# fueltype    16.19343    0.00411582
next.iteration(model_1.3,5)# very low p-value   
# Variable          VIF       P-value
# enginesize    16.13411    0.002422654

#comparatively carbodysedan( 0.02846494) and carbodyhatchback(0.02052965) have higher p values
# let's remove them one by one and see the impact

##################################
#removing carbodysedan 
model_1.4 <- lm(price ~ symboling + fueltype + aspiration + enginelocation + 
                  wheelbase + carlength + carwidth + carheight + curbweight + 
                  enginesize + compressionratio  + citympg + carbodyhardtop + 
                  carbodyhatchback + carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                  fuelsystemmpfi + Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarisuzu + Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)
summary(model_1.4)
#R-squared:  0.9815,	Adjusted R-squared:  0.9745 
#no impact on adjusted r-square
#before we drop carbodyhatchback as well let's analysys the new model
vif(model_1.4)
#carbodyhatchback vif value has gone significatluy down(2.601754) we will keep it for the moment 2.601754
####################################
next.iteration(model_1.4,0)   
# Variable                VIF       P-value 
# cylindernumberfour    23.86083    0.0182251
#p-value of cylindernumberfour is fluctuating in each model(prev was 0.007067993) signifying its dependet on other varaible ;
# signified by its vif factor to , now its can didate for dropping
  
model_1.5 <- lm(price ~ symboling + fueltype + aspiration + enginelocation + 
                  wheelbase + carlength + carwidth + carheight + curbweight + 
                  enginesize + compressionratio  + citympg + carbodyhardtop + 
                  carbodyhatchback + carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive  + 
                  cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                  fuelsystemmpfi + Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarisuzu + Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)

summary(model_1.5)
#R-squared:  0.9805,	Adjusted R-squared:  0.9733 
#minimal impact on adjusted r sqaure
#lets conitnue with next interaction
next.iteration(model_1.5,0)   
# Variable        VIF       P-value 
# wheelbase    23.86083    0.0004642035
#p value is very significant lets try teh next higheste VIF
next.iteration(model_1.5,1)   
# Variable        VIF       P-value 
# enginesize    15.88531    0.003721405
#still p value is low
next.iteration(model_1.5,2)   
# Variable        VIF       P-value 
# fueltype    15.51375    0.001187228
#still p value is low
next.iteration(model_1.5,3)   
# Variable        VIF       P-value 
# carwidth    15.05195    0.1171388
# high vif and p value signifies that carwidth is a candidate for removal


model_1.6 <- lm(price ~ symboling + fueltype + aspiration + enginelocation + 
                  wheelbase + carlength + carheight + curbweight + 
                  enginesize + compressionratio  + citympg + carbodyhardtop + 
                  carbodyhatchback + carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive  + 
                  cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                  fuelsystemmpfi + Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarisuzu + Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)
summary(model_1.6)
#R-squared:   0.98,	Adjusted R-squared:  0.973 
#adjusted 4 square is mostly un affected
#lets continue our iterations
next.iteration(model_1.6,0)  
# Variable        VIF       P-value 
# fueltype    15.40393    0.001877711
#very significiant p value
next.iteration(model_1.6,1)
# Variable        VIF       P-value 
# enginesize    15.27506    0.00124308
#very significiant p value
next.iteration(model_1.6,2)
# Variable        VIF       P-value 
# curbweight    12.84905    0.000708167
#very significiant p value
next.iteration(model_1.6,3)
# Variable        VIF       P-value 
# wheelbase    12.81267    3.894085e-06
#very significiant p value
next.iteration(model_1.6,4)
next.iteration(model_1.6,5)
next.iteration(model_1.6,6)
next.iteration(model_1.6,7)
next.iteration(model_1.6,8)
#4-8 all had significant p-values
next.iteration(model_1.6,9)
# Variable            VIF       P-value 
# fuelsystemmpfi    6.265379    0.1309551
#fuelsystemmpfi is candidate for removal
model_1.7 <- lm(price ~ symboling + fueltype + aspiration + enginelocation + 
                  wheelbase + carlength + carheight + curbweight + 
                  enginesize + compressionratio  + citympg + carbodyhardtop + 
                  carbodyhatchback + carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive  + 
                  cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                   Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarisuzu + Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)
summary(model_1.7)
#R-squared:  0.9796,	Adjusted R-squared:  0.9726 
# minimal impact on Adjusted R-squared
#continuing with iteration of vif follwed by p value
next.iteration(model_1.7,0)
next.iteration(model_1.7,1)
next.iteration(model_1.7,2)
next.iteration(model_1.7,3)
next.iteration(model_1.7,4)
next.iteration(model_1.7,5)
next.iteration(model_1.7,6)
next.iteration(model_1.7,7)
next.iteration(model_1.7,8)
next.iteration(model_1.7,9)
next.iteration(model_1.7,10)
next.iteration(model_1.7,11)
next.iteration(model_1.7,12)
next.iteration(model_1.7,13)
next.iteration(model_1.7,14)
next.iteration(model_1.7,15)
next.iteration(model_1.7,16)
#all hand significant p-values
next.iteration(model_1.7,17)
# Variable            VIF       P-value 
# fuelsystem2bbl    3.200508    0.1978278
#fuelsystem2bbl is candidate for removal
model_1.8 <- lm(price ~ symboling + fueltype + aspiration + enginelocation + 
                  wheelbase + carlength + carheight + curbweight + 
                  enginesize + compressionratio  + citympg + carbodyhardtop + 
                  carbodyhatchback + carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarisuzu + Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)
summary(model_1.8)
#R-squared:  0.9792,	Adjusted R-squared:  0.9724 
# minimal impact on Adjusted R-squared
#continuing with iteration of vif follwed by p value
next.iteration(model_1.8,0)
next.iteration(model_1.8,1)
# Variable            VIF       P-value 
# compressionratio    12.32534    0.01303874
#compressionratio the p-value is higher than most lest see if its removal has impatc on our adjusted R2 value
model_1.9 <- lm(price ~ symboling + fueltype + aspiration + enginelocation + 
                  wheelbase + carlength + carheight + curbweight + 
                  enginesize  + citympg + carbodyhardtop + 
                  carbodyhatchback + carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarisuzu + Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)
summary(model_1.9)
#Multiple R-squared:  0.978,	Adjusted R-squared:  0.9711
#impact on Adjusted R-squared is greater than before but still marginal
#lets continue with our iterations
next.iteration(model_1.9,0)
next.iteration(model_1.9,1)
next.iteration(model_1.9,2)
next.iteration(model_1.9,3)
next.iteration(model_1.9,4)
next.iteration(model_1.9,5)
next.iteration(model_1.9,6)
next.iteration(model_1.9,7)
next.iteration(model_1.9,8)
# Variable        VIF       P-value 
# carheight    4.524745    0.02573848
#carheight the p-value is higher than most lest see if its removal has impatc on our adjusted R2 value
model_2.1 <- lm(price ~ symboling + fueltype + aspiration + enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + carbodyhardtop + 
                  carbodyhatchback + carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarisuzu + Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)
summary(model_2.1)
#R-squared:  0.977,	Adjusted R-squared:   0.97 
#imapct on adjusted r2 is accpetable, lets continue with teh iteration
next.iteration(model_2.1,0)
next.iteration(model_2.1,1)
next.iteration(model_2.1,2)
next.iteration(model_2.1,3)
next.iteration(model_2.1,4)
next.iteration(model_2.1,5)
next.iteration(model_2.1,6)
next.iteration(model_2.1,7)
next.iteration(model_2.1,8)
next.iteration(model_2.1,9)
next.iteration(model_2.1,10)
next.iteration(model_2.1,11)
next.iteration(model_2.1,12)
# Variable        VIF       P-value 
# fueltype    3.786085    0.4144783
#very high p -value cna be removed definately
model_2.2 <- lm(price ~ symboling  + aspiration + enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + carbodyhardtop + 
                  carbodyhatchback + carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarisuzu + Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)
summary(model_2.2)
#impact on adjusted R2 is acceptable
next.iteration(model_2.2,0)
next.iteration(model_2.2,1)
next.iteration(model_2.2,2)
next.iteration(model_2.2,3)
next.iteration(model_2.2,4)
next.iteration(model_2.2,5)
next.iteration(model_2.2,6)
next.iteration(model_2.2,7)
next.iteration(model_2.2,8)
next.iteration(model_2.2,9)
next.iteration(model_2.2,10)
next.iteration(model_2.2,11)
next.iteration(model_2.2,12)
next.iteration(model_2.2,13)
next.iteration(model_2.2,14)
next.iteration(model_2.2,15)
# Variable              VIF       P-value 
# carbodyhatchback    2.190304    0.9991908
#very high p -value cna be removed definately
model_2.3 <- lm(price ~ symboling  + aspiration + enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + carbodyhardtop + 
                   carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarisuzu + Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)

summary(model_2.3)
# R-squared:  0.9768,	Adjusted R-squared:  0.9703
#impact on R2 isaccpetable
vif(model_2.3)
#lets try the iteration 
next.iteration(model_2.3,0)
next.iteration(model_2.3,1)
next.iteration(model_2.3,2)
next.iteration(model_2.3,3)
next.iteration(model_2.3,4)
next.iteration(model_2.3,5)
next.iteration(model_2.3,6)
next.iteration(model_2.3,7)
next.iteration(model_2.3,8)
next.iteration(model_2.3,9)
next.iteration(model_2.3,10)
next.iteration(model_2.3,11)
next.iteration(model_2.3,12)
next.iteration(model_2.3,13)
next.iteration(model_2.3,14)
#seems like most of te high vif variavle also have signficant p value

#lets inverse the staregy for sometime now
#let's pick up the lowest p-values and see their impact on adjusted r-squared
summary(model_2.3)
#Manufacturarisuzu with 0.343889 as p value is teh highst, let's remove it
model_2.4 <- lm(price ~ symboling  + aspiration + enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + carbodyhardtop + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)
summary(model_2.4)
#R-squared:  0.9766,	Adjusted R-squared:  0.9704 
#minimal impact on adjusted R2
#carbodyhardtop with 0.330138 as p vlaue is a candidate for droping
model_2.5 <- lm(price ~ symboling  + aspiration + enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)
summary(model_2.5)
#R-squared:  0.9764,	Adjusted R-squared:  0.9704 
#as expected no imapct on R2
#cylindernumberfive with p value as 0.053046 is a candidate for removal
model_2.6 <- lm(price ~ symboling  + aspiration + enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota + Manufacturarvolkswagen, data = train)
summary(model_2.6)
vif(model_2.6)
#Manufacturarvolkswagen with p value 0.009185 is one of the highest lest remove that
model_2.7 <- lm(price ~ symboling  + aspiration + enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota , data = train)
summary(model_2.7)
#R-squared:  0.9741,	Adjusted R-squared:  0.9681
# the drop in adjusted R2 is accpetable
#symboling with an p value of 0.031847 is a candidate for dropping'
model_2.8 <- lm(price ~aspiration + enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturarsaab + Manufacturartoyota , data = train)
summary(model_2.8)
#R-squared:  0.9731,	Adjusted R-squared:  0.967 
# the drop in adjusted R2 is accpetable
#Manufacturarsaab with p value 0.050107 is the new candidate for drop
model_2.9 <- lm(price ~aspiration + enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarmazda + Manufacturarmitsubishi + 
                  ManufacturarNissan + Manufacturarplymouth + Manufacturarrenault + 
                   Manufacturartoyota , data = train)
summary(model_2.9)
#R-squared:  0.9722,	Adjusted R-squared:  0.9662
#minmal drop in r2
#ManufacturarNissan with p value 0.036866 is the next candidate for drop
model_3.0 <- lm(price ~aspiration + enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarmazda + Manufacturarmitsubishi + 
                   Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturartoyota , data = train)
summary(model_3.0)
# R-squared:  0.9711,	Adjusted R-squared:  0.9652 
#minimal drop in adjusted R2
#aspiration with p value 0.024469 is the next candidate for dropping
model_3.1 <- lm(price ~ enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv  + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarmazda + Manufacturarmitsubishi + 
                  Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturartoyota , data = train)
summary(model_3.1)
#R-squared:  0.9698,	Adjusted R-squared:  0.964
#again acceptable drop in adjusted R2 
#enginetypeohcv with p vlaue 0.012115 is the next candidate for drop
model_3.2 <- lm(price ~ enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick + Manufacturardodge + 
                  Manufacturarmazda + Manufacturarmitsubishi + 
                  Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturartoyota , data = train)
summary(model_3.2)
#R-squared:  0.9682,	Adjusted R-squared:  0.9623
#slightly greater but acceptable drop in adjusted R2
#Manufacturardodge with  0.009263 is teh next candidate
model_3.3 <- lm(price ~ enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick  + 
                  Manufacturarmazda + Manufacturarmitsubishi + 
                  Manufacturarplymouth + Manufacturarrenault + 
                  Manufacturartoyota , data = train)
summary(model_3.3)
#R-squared:  0.9663,	Adjusted R-squared:  0.9605 
#acceptable drop iin adjusted R2
#Manufacturarplymouth with p value 0.018668 is the next candidate to drop
model_3.4 <- lm(price ~ enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick  + 
                  Manufacturarmazda + Manufacturarmitsubishi + 
                   Manufacturarrenault + 
                  Manufacturartoyota , data = train)
summary(model_3.4)
#R-squared:  0.9647,	Adjusted R-squared:  0.959 
#acceptable drop
#Manufacturarmitsubishi with p-value 0.033376 is teh next candidate for drop
model_3.5 <- lm(price ~ enginelocation + 
                  wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + 
                  carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  cylindernumbersix + cylindernumberthree  + 
                  Manufacturarbmw + Manufacturarbuick  + 
                  Manufacturarmazda +  
                  Manufacturarrenault + 
                  Manufacturartoyota , data = train)
summary(model_3.5)
#R-squared:  0.9634,	Adjusted R-squared:  0.9577
#acceptable drop in adjusted R2
#Manufacturarmazda with p-value 0.040702 is teh next candidate to drop
model_3.6 <- lm(price ~ enginelocation +wheelbase + carlength  + curbweight + 
                  enginesize  + citympg + carbodywagon + drivewheelrwd + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  cylindernumbersix + cylindernumberthree  + Manufacturarbmw + Manufacturarbuick  + 
                  Manufacturarrenault + Manufacturartoyota , data = train)
summary(model_3.6)
#R-squared:  0.9621,	Adjusted R-squared:  0.9566
#acceptable drop in adjusted R2
#curbweight with p-value 0.016825 is teh next candidate for drop
#its continueos variable bein consider for drop after a while
model_3.7 <- lm(price ~ enginelocation +wheelbase + carlength +enginesize  + citympg
                + carbodywagon + drivewheelrwd +enginetypedohcv + enginetypel + enginetypeohc
                + enginetypeohcf + cylindernumbersix + cylindernumberthree  + Manufacturarbmw
                + Manufacturarbuick  + Manufacturarrenault + Manufacturartoyota , data = train)
summary(model_3.7)
#R-squared:  0.9603,	Adjusted R-squared:  0.9549
#acceptable drop in adjusted R2
#Manufacturarrenault with p-value  0.018086 is teh next candidate
model_3.8 <- lm(price ~ enginelocation +wheelbase + carlength +enginesize  + citympg
                + carbodywagon + drivewheelrwd +enginetypedohcv + enginetypel + enginetypeohc
                + enginetypeohcf + cylindernumbersix + cylindernumberthree  + Manufacturarbmw
                + Manufacturarbuick + Manufacturartoyota , data = train)
summary(model_3.8)
#R-squared:  0.9585,	Adjusted R-squared:  0.9532
#acceptable drop in adjusted R2
#cylindernumberthree with p-value 0.009723 is the next candidate
model_3.9 <- lm(price ~ enginelocation +wheelbase + carlength +enginesize  + citympg
                + carbodywagon + drivewheelrwd +enginetypedohcv + enginetypel + enginetypeohc
                + enginetypeohcf + cylindernumbersix  + Manufacturarbmw
                + Manufacturarbuick + Manufacturartoyota , data = train)
summary(model_3.9)
#R-squared:  0.9562,	Adjusted R-squared:  0.9511
#acceptable drop in adjusted R2
#cylindernumbersix with p-value 0.008684 is the next candidate
model_4.0 <- lm(price ~ enginelocation +wheelbase + carlength +enginesize+citympg+ carbodywagon 
                + drivewheelrwd +enginetypedohcv + enginetypel + enginetypeohc
                + enginetypeohcf+ Manufacturarbmw+ Manufacturarbuick + Manufacturartoyota , data = train)
summary(model_4.0)
#R-squared:  0.9538,	Adjusted R-squared:  0.9487
#acceptable drop in adjusted R2
#Manufacturartoyota with 0.003466 is teh next candidate for drop
model_4.1 <- lm(price ~ enginelocation +wheelbase + carlength +enginesize+citympg+ carbodywagon 
                + drivewheelrwd +enginetypedohcv + enginetypel + enginetypeohc
                + enginetypeohcf+ Manufacturarbmw+ Manufacturarbuick  , data = train)
summary(model_4.1)
#R-squared:  0.9506,	Adjusted R-squared:  0.9456 
#slightly bigger drop but still accetable
#enginetypeohcf with 0.004968 is next candidate for drop
model_4.2 <- lm(price ~ enginelocation +wheelbase + carlength +enginesize+citympg+ carbodywagon 
                + drivewheelrwd +enginetypedohcv + enginetypel + enginetypeohc
                + Manufacturarbmw+ Manufacturarbuick  , data = train)
summary(model_4.2)
#R-squared:  0.9475,	Adjusted R-squared:  0.9426 
#slightly bigger drop but still accetable
# enginetypel with p-value 0.034515 is next candidate
model_4.3 <- lm(price ~ enginelocation +wheelbase + carlength +enginesize+citympg+ carbodywagon 
                + drivewheelrwd +enginetypedohcv + enginetypeohc
                + Manufacturarbmw+ Manufacturarbuick  , data = train)
summary(model_4.3)
# R-squared:  0.9456,	Adjusted R-squared:  0.9411 
#acceptable drop
#wheelbase with p-value 0.042819 is the next candidate for drop
#it's continuous varaible being consder for drop after a while
model_4.4 <- lm(price ~ enginelocation + carlength +enginesize+citympg+ carbodywagon 
                + drivewheelrwd +enginetypedohcv + enginetypeohc
                + Manufacturarbmw+ Manufacturarbuick  , data = train)
summary(model_4.4)
#  R-squared:  0.9439,	Adjusted R-squared:  0.9396
# acceptable drop
# enginetypeohc with p -value 0.004462 is the next candidate
model_4.5 <- lm(price ~ enginelocation + carlength +enginesize+citympg+ carbodywagon 
                + drivewheelrwd +enginetypedohcv + Manufacturarbmw+ Manufacturarbuick, data = train)
summary(model_4.5)
#R-squared:  0.9403,	Adjusted R-squared:  0.9363
#slightly bigger by acceptabel drop
#drivewheelrwd has becoem less signficant with p-value 0.008982 in this model and is teh next candidate
model_4.6 <- lm(price ~ enginelocation + carlength +enginesize+citympg+ carbodywagon 
                +enginetypedohcv + Manufacturarbmw+ Manufacturarbuick, data = train)
summary(model_4.6)
#R-squared:  0.9372,	Adjusted R-squared:  0.9334
#slightly bigger by acceptabel drop
#All varaible are now higly significant
#lets check if our next iteration has any recomdation for drop
vif(model_4.6)
next.iteration(model_4.6,0)
#varaiable      vif         p-value
# enginesize    3.171185  9.361634e-14
#very significant
#lets try dropping  carbodywagon whihc has the highest p value and see its impact on our model
next.iteration(model_4.6,1)

model_4.7 <- lm(price ~ enginelocation + carlength +citympg+ enginesize 
                +enginetypedohcv + Manufacturarbmw+ Manufacturarbuick, data = train)
summary(model_4.7)
#R-squared:  0.9311,	Adjusted R-squared:  0.9275  
# Acceptable drop 
vif(model_4.7)
#all vif are enginesize is still above 3, let's remove and see its impact
model_4.7.1 <- lm(price ~ enginelocation + carlength +citympg 
                +enginetypedohcv + Manufacturarbmw+ Manufacturarbuick, data = train)
summary(model_4.7.1)
#R-squared:  0.8932,	Adjusted R-squared:  0.8885 
vif(model_4.7.1)


#lets compare results of 4.6,4.7 and 4.7.1

Predict_4.6 <- predict(model_4.6,test[,-19])
Predict_4.7 <- predict(model_4.7,test[,-19])
Predict_4.7.1 <- predict(model_4.7.1,test[,-19])

test$error_model4.6 <- test[,19] - Predict_4.6
test$error_model4.7 <- test[,19] - Predict_4.7
test$error_model4.7.1 <- test[,19] - Predict_4.7.1
test$Predict_4.6<-Predict_4.6
test$Predict_4.7<-Predict_4.7
test$Predict_4.7.1<-Predict_4.7.1

r_4.6 <- cor(test[,19],Predict_4.6)
rsquared_4.6 <- cor(test[,19],Predict_4.6)^2
rsquared_4.6#0.8298251

r_4.7 <- cor(test[,19],Predict_4.7)
rsquared_4.7 <- cor(test[,19],Predict_4.7)^2
rsquared_4.7#0.8267555 slightly lower


r_4.7.1 <- cor(test[,19],Predict_4.7.1)
rsquared_4.7.1 <- cor(test[,19],Predict_4.7.1)^2
rsquared_4.7.1#0.7478413 predcition drop is fairly signifcant


#Lets move back to analyzing 4.7
summary(model_4.7)
vif(model_4.7)
#enginetypedohcv is the odd one , lets try and remove and see its impact 
model_4.8 <- lm(price ~ enginelocation + carlength + enginesize +citympg 
                 + Manufacturarbmw+ Manufacturarbuick, data = train)
summary(model_4.8)
#R-squared:  0.922,	Adjusted R-squared:  0.9186 
#minimal drop in R2
#lets check its impact on predicted R2
Predict_4.8 <- predict(model_4.8,test[,-19])
cor(test[,19],Predict_4.8)^2#0.8295618
#predicted R2 is similar we cna keep this model
vif(model_4.8)

#we were not able to remove enginesize, lets see if enginelocation beingremoved has an impact
model_4.9 <- lm(price ~  carlength + enginesize +citympg 
                + Manufacturarbmw+ Manufacturarbuick, data = train)
summary(model_4.9)
#R-squared:  0.8744,	Adjusted R-squared:  0.8699
#slightly bigger drop in R2
# Manufacturarbmw and carlength has lost their significane with this chnage
#lets check its impact on predicted R2
Predict_4.9 <- predict(model_4.9,test[,-19])
cor(test[,19],Predict_4.9)^2#0.8257606
#Minmal drop in predicted R2 , we cna drop this variable too
vif(model_4.9)
summary(model_4.9)
#lets check nect candidate for drop from thsi model
next.iteration(model_4.9,0)
# Varaible    vif      p-value
#enginesize  2.503631  7.847204e-20

model_5.0 <- lm(price ~  carlength  +citympg 
                + Manufacturarbmw+ Manufacturarbuick, data = train)
summary(model_5.0)
#R-squared:  0.7692,	Adjusted R-squared:  0.7625 
#very significat drop in R2
#lets try the next one
next.iteration(model_4.9,1)
# Varaible    vif      p-value
#citympg  2.191085  2.037186e-09
#lets remove citympg and see teh impact
model_5.1 <- lm(price ~  carlength + Manufacturarbmw+ Manufacturarbuick, data = train)
summary(model_5.1)
#R-squared:  0.5982,	Adjusted R-squared:  0.5895
#very significant drop again

######################################################
#we will stick with model 4.6,4.7,4.8 and 4.9 for further validations
str(test)

test$generatedID <- seq.int(nrow(test))
#model 4.6
# Actual vs predicted values from model 4.6 
ggplot(test, aes(generatedID, price)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "generatedID") + 
  scale_y_continuous(name = "price") +
  geom_line(aes(x=generatedID, y=Predict_4.6, colour="red"))
#the data pattern is follwed but the peaks in the predcited data are higher

# Plot model_4.6 errors
ggplot(test, aes(generatedID, error_model4.6)) + geom_line() + 
  scale_x_continuous(name = "generatedID") + 
  scale_y_continuous(name = "Error") + 
  geom_hline(yintercept = 0)
# error seems to be random

#Model 4.7
# Actual vs predicted values from model 4.7 
ggplot(test, aes(generatedID, price)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "generatedID") + 
  scale_y_continuous(name = "price") +
  geom_line(aes(x=generatedID, y=Predict_4.7, colour="red"))
# seems to be similar to 4.6 but teh intial drops are not captured lest plot all three to verify

ggplot(test, aes(generatedID, price)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "generatedID") + 
  scale_y_continuous(name = "price") +
  geom_line(aes(x=generatedID, y=Predict_4.7, colour="red"))+
  geom_line(aes(x=generatedID, y=Predict_4.6, colour="green"))
#we can clearly see that teh green curve of model 4.6 fits the blue curve of original prices better

#error of model 4.7
ggplot(test, aes(generatedID, error_model4.7)) + geom_line() + 
  scale_x_continuous(name = "generatedID") + 
  scale_y_continuous(name = "Error") + 
  geom_hline(yintercept = 0)
# error seems random here as well


###################################################
#Model 4.8
# Actual vs predicted values from model 4.8 
ggplot(test, aes(generatedID, price)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "generatedID") + 
  scale_y_continuous(name = "price") +
  geom_line(aes(x=generatedID, y=Predict_4.8, colour="red"))
# seems to follow the prices liek model 4.6, lets compare them directly
ggplot(test, aes(generatedID, price)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "generatedID") + 
  scale_y_continuous(name = "price") +
  geom_line(aes(x=generatedID, y=Predict_4.8, colour="red"))+
  geom_line(aes(x=generatedID, y=Predict_4.6, colour="green"))
#we can clearly see that teh green curve of model 4.6 fits the blue curve of original prices better

#error of model 4.8
ggplot(test, aes(generatedID, (price-Predict_4.8))) + geom_line() + 
  scale_x_continuous(name = "generatedID") + 
  scale_y_continuous(name = "Error") + 
  geom_hline(yintercept = 0)
# error seems random here as well

##############################################################

#Model 4.9
# Actual vs predicted values from model 4.9 
ggplot(test, aes(generatedID, price)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "generatedID") + 
  scale_y_continuous(name = "price") +
  geom_line(aes(x=generatedID, y=Predict_4.9, colour="red"))
# seems to follow the prices liek model 4.6, lets compare them directly
ggplot(test, aes(generatedID, price)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "generatedID") + 
  scale_y_continuous(name = "price") +
  geom_line(aes(x=generatedID, y=Predict_4.9, colour="red"))+
  geom_line(aes(x=generatedID, y=Predict_4.6, colour="green"))
#we can clearly see that teh green curve of model 4.6 fits the blue curve of original prices better

#error of model 4.9
ggplot(test, aes(generatedID, (price-Predict_4.9))) + geom_line() + 
  scale_x_continuous(name = "generatedID") + 
  scale_y_continuous(name = "Error") + 
  geom_hline(yintercept = 0)
# error seems random here as well


##################################################


#lets use boot straping to test the stability of the model
#moving forward using model 4.6,4.8,4.9
###############

B = 1000 ## number of bootstraps
results_4.6_test = numeric(B) ## vector to hold results
results_4.8_test = numeric(B)
results_4.9_test = numeric(B)
results_4.6_total = numeric(B) ## vector to hold results
results_4.8_total = numeric(B)
results_4.9_total = numeric(B)

for(b in 1:B){
  i = sample(x = 1:nrow(test), size = 35, replace = F) ## sample indices
  j = sample(x = 1:nrow(Data.Cars), size = 35, replace = F) ## sample indices
  bootSample_test = test[i,] ## get data
  bootSample_whole = Data.Cars[j,]
  #test
  bootSample_test$predict_4.6<-predict(model_4.6, bootSample_test[,-19])
  bootSample_test$predict_4.8<-predict(model_4.8, bootSample_test[,-19])
  bootSample_test$predict_4.9<-predict(model_4.9, bootSample_test[,-19])
  #Data.Cars
  bootSample_whole$predict_4.6<-predict(model_4.6, bootSample_whole[,-19])
  bootSample_whole$predict_4.8<-predict(model_4.8, bootSample_whole[,-19])
  bootSample_whole$predict_4.9<-predict(model_4.9, bootSample_whole[,-19])
  
  ## test
  results_4.6_test[b] = cor(bootSample_test$price,bootSample_test$predict_4.6)^2 ## store results
  results_4.8_test[b] = cor(bootSample_test$price,bootSample_test$predict_4.8)^2
  results_4.9_test[b] = cor(bootSample_test$price,bootSample_test$predict_4.9)^2
  
  #Data.cars
  results_4.6_total[b] = cor(bootSample_whole$price,bootSample_whole$predict_4.6)^2 ## store results
  results_4.8_total[b] = cor(bootSample_whole$price,bootSample_whole$predict_4.8)^2
  results_4.9_total[b] = cor(bootSample_whole$price,bootSample_whole$predict_4.9)^2
}

#analyzing 4.8 first
#for Test
mean(results_4.8_test)# 0.8286488
#for Complete data
mean(results_4.8_total)#0.8898435

#analyzing 4.9 next
mean(results_4.9_test)# 0.8232618
#for Complete data
mean(results_4.9_total)#0.8532233
#Model 4.9 is not able to predict the right values

#analyzing 4.6
mean(results_4.6_test)# 0.8295654
#for Complete data
mean(results_4.6_total)#0.9021299

summary(model_4.9)# 4(5,2 manufacturer) #carlength and Manufacturarbmw have lost their significace
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         9246.9     1768.0   5.230 6.19e-07 ***
#   carlength            530.9      158.4   3.351  0.00104 ** 
#   enginesize          2565.5      239.5  10.714  < 2e-16 ***
#   citympg            -1058.7      164.8  -6.423 2.04e-09 ***
#   Manufacturarbmw     4624.2     1762.5   2.624  0.00969 ** 
#   Manufacturarbuick   6113.9     1412.7   4.328 2.89e-05 ***
summary(model_4.6)#7(8,2 manufacturer)  all highily significant
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         8432.6     1267.2   6.654 6.65e-10 ***
#   enginelocation     14959.8     1384.9  10.802  < 2e-16 ***
#   carlength           1055.0      124.8   8.451 4.27e-14 ***
#   enginesize          1601.8      192.8   8.310 9.36e-14 ***
#   citympg             -898.4      118.7  -7.568 5.45e-12 ***
#   carbodywagon       -1981.7      549.6  -3.606 0.000438 ***
#   enginetypedohcv    10230.5     2260.6   4.525 1.32e-05 ***
#   Manufacturarbmw     6378.0     1275.1   5.002 1.75e-06 ***
#   Manufacturarbuick   9077.0     1049.5   8.649 1.41e-14 ***

summary(model_4.8)#5(6,2 manufacturar) all highily significant
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         8506.0     1400.8   6.072 1.19e-08 ***
#   enginelocation     13740.1     1508.4   9.109 9.40e-16 ***
#   carlength            821.1      129.3   6.351 2.99e-09 ***
#   enginesize          1965.5      200.5   9.802  < 2e-16 ***
#   citympg             -932.0      131.1  -7.108 6.03e-11 ***
#   Manufacturarbmw     6055.6     1402.9   4.316 3.03e-05 ***
#   Manufacturarbuick   8212.6     1140.9   7.198 3.74e-11 ***


#model_4.6 is  slightly better 
#but their are two/three less variable in 4.8/4.9; 
#we will choose  4.8 as our model as 4.9 has similar prediction error but its variable become less significant 
# ALso the variable  enginelocation is the not present in 4.9 , we will give it less priority 



summary(model_4.8)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         8506.0     1400.8   6.072 1.19e-08 ***
#   enginelocation     13740.1     1508.4   9.109 9.40e-16 ***
#   carlength            821.1      129.3   6.351 2.99e-09 ***
#   enginesize          1965.5      200.5   9.802  < 2e-16 ***
#   citympg             -932.0      131.1  -7.108 6.03e-11 ***
#   Manufacturarbmw     6055.6     1402.9   4.316 3.03e-05 ***
#   Manufacturarbuick   8212.6     1140.9   7.198 3.74e-11 ***



#car length and engien size is significant, pointing to the usage of the car
#bigger car can vary in engine sizes; with smaller engine being family wagon heavier engine being a sports car or luxury car

#Brand name particularly Buick and BMW can garner bigger prices; may be a tie up with them will suit us.

#milage in city is a deciding factor too


#Engine location seems to be a factor in deciding prices to an extent too,
# as mostly the super cars have bigger engine located behind (we set rear as 1) , the positive co-efficient signifies that

