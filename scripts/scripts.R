
library(ggplot2)
library(tidyverse)


#1. Import data

#The data was taken from the following location, 
#https://confluence.csiro.au/display/DataSchool/Visualisation

# Mutton Flavour
# Fatty acids including MOA, EOA and MNA accumulate in lamb meat and can make meat from older animals taste unpleasant. 
# Another fatty acid, C17:0, also accumlates with age although is not responsible for the unpleasant 'mutton flavour'. 
# C17 is easier to measure than MOA, EOA or MNA; 
# this study investigates whether C17 could be a useful 'proxy' for indicating how 'muttony' meat is likely to taste. 

data <- read_csv("./data/bcfa_C17_fa.csv")

str(data) #structure not STRING

view(data)



#2. Visualise the data

#nb. use rm() to remove a variable form the global environment



#initial look

gath <- gather(data2, key="Measurement", value="Amount", c("MOA", "MNA", "EOA", "C17.0"))


ggplot(gath, aes(x=Year, y=Amount, colour=Measurement)) +
  geom_point()

#They are grouped into only 2 years so jitter the lines 

ggplot(gath, aes(x=Year, y=Amount, colour=Measurement)) +
  geom_point() + geom_jitter()



# second attempt

#add a sample id column so we can still identify which measurents match up to the same sample.
data2 <- mutate(data, sampleId = 1:212)


gath <- gather(data2, key="Measurement", value="Amount", c("MOA", "MNA", "EOA"))


ggplot(gath, aes(x=C17.0, y=Amount, colour=Measurement)) + geom_point()

#split based on year
ggplot(gath, aes(x=C17.0, y=Amount, colour=Measurement)) + geom_point() + facet_wrap(~Year)





#split based on year
gath_2011 <- filter(gath, Year==2011)
gath_2014 <- filter(gath, Year==2014)



# Filter out the years

data_2011 <- filter(data, Year==2011)
data_2014 <- filter(data, Year==2014)



# Plot C17.0 against each of the other measurements

ggplot(data = data_2011, mapping = aes(x=C17.0)) + 
  geom_point(aes(y=MOA, colour="red") ) +
  geom_point(aes(y=MNA, colour="green") ) +
  geom_point(aes(y=EOA, colour="blue") ) 
  

ggplot(data = data_2014, mapping = aes(x=C17.0)) + 
  geom_point(aes(y=MOA), colour="red") +
  geom_point(aes(y=MNA), colour="green" ) +
  geom_point(aes(y=EOA), colour="blue" ) 

#nb. ALT + UP OR DOWN ARROW MOVES ENTIRE ROW UP OR DOWN.

ggplot(data = data_2011, mapping = aes(x=C17.0, y=MOA)) +geom_point()
ggplot(data = data_2011, mapping = aes(x=C17.0, y=MNA)) +geom_point()
ggplot(data = data_2011, mapping = aes(x=C17.0, y=EOA)) +geom_point()


ggplot(data = data_2014, mapping = aes(x=C17.0, y=MOA)) +geom_point()
ggplot(data = data_2014, mapping = aes(x=C17.0, y=MNA)) +geom_point()
ggplot(data = data_2014, mapping = aes(x=C17.0, y=EOA)) +geom_point()




# Blocking based upon year.


# 3. Fit a Model

library(lmerTest)

# From visualisation 2014 seems like a better year to find relationships in, so start there.

lm_MOA_2014 <- lm(MOA ~ C17.0, data = data_2014)
lm_MNA_2014 <- lm(MNA ~ C17.0, data = data_2014)
lm_EOA_2014 <- lm(EOA ~ C17.0, data = data_2014)


# Asses Model Assumptions for 2014

anova(lm_MOA_2014)
anova(lm_MNA_2014)
anova(lm_EOA_2014)


summary(lm_MOA_2014)
summary(lm_MNA_2014)
summary(lm_EOA_2014)


plot(lm_MOA_2014)
plot(lm_MNA_2014)
plot(lm_EOA_2014)







# Now do 2011

lm_MOA_2011 <- lm(MOA ~ C17.0, data = data_2011)
lm_MNA_2011 <- lm(MNA ~ C17.0, data = data_2011)
lm_EOA_2011 <- lm(EOA ~ C17.0, data = data_2011)

# Asses Model Assumptions for 2011

anova(lm_MOA_2011)
anova(lm_MNA_2011)
anova(lm_EOA_2011)


summary(lm_MOA_2011)
summary(lm_MNA_2011)
summary(lm_EOA_2011)


plot(lm_MOA_2011)
plot(lm_MNA_2011)
plot(lm_EOA_2011)







# Now test to see if there are any additive relationships between MOA, MNA, EOA, and C17.0.
# use Year as a blocking factor
library(lmerTest)

lmer_add <- lmer(C17.0~MOA+MNA+EOA +(1|Year), data=data)

#Assess model assumptions
anova(lmer_add)
summary(lmer_add)
plot(lmer_add)




# Now test to see if there are any full factor relationships between MOA, MNA, EOA, and C17.0 in any combination or interactions.
lmer_fullfactor <- lmer(C17.0~MOA*MNA*EOA +(1|Year), data=data)

#Assess model assumptions
anova(lmer_fullfactor)
summary(lmer_fullfactor)
plot(lmer_fullfactor)

#Doesn't look like any relationships between MOA, MNA, and EOA. 




#Got a warning about the scales being wrong so apply a log scale to MOA based upon the following graph
ggplot(gath, aes(x=C17.0, y=Amount, colour=Measurement)) + geom_point()

lmer_add_log <- lmer(C17.0~log(MOA)+MNA+EOA +(1|Year), data=data)
#well that didn't work, try the other model,

lmer_fullfactor_log <- lmer(C17.0~log(MOA)*MNA*EOA +(1|Year), data=data)
#that didn't work either?



ggplot(data = data_2011, mapping = aes(x=C17.0, y=log(MOA))) +geom_point()
ggplot(data = data_2011, mapping = aes(x=C17.0, y=log(MNA))) +geom_point()
ggplot(data = data_2011, mapping = aes(x=C17.0, y=EOA)) +geom_point()

#Conclusion
#----------
#2014 levels of C17 are way lower than 2011 levels.
#this would seem to indicate that 2011 animals are much older than the 2014 animals.
#So C17 is a good predictor of MOA, MDA and EOA in young animals but not in older animals.


# Was this distructive sampling of the animals? If not, and same animals used in 2014 then a persistant animalId would have been good when data was recorded.
# So that we could do some blocking, and figure out what are replicates.
# Animal age might have been good too.


library(emmeans)
emmeans(lmer_add, pairwise~Year)