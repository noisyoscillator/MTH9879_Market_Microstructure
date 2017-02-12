orderSets<- function(Lalpha, Lmu, Ldelta, NumE){

    # inputs: sets of parameters
    
    # Get the length of input lists: number of sets
    n<- length(Lalpha)
    print(n)
    # Generate 100,000 evenets with the 4 sets parameters
    NumEvents<- NumE # Average over 100,000 events

    AvgBookShapes<- c() # Empty list
    
    for(i in 1:n){
        alpha<-Lalpha[i]
        mu<-Lmu[i]
        delta<-Ldelta[i]
        print(delta)
        # Initialize the order book with initializeBook5() function 
        # with asymptotic depth of 5 shares
        initializeBook5()
        # Burn in for 100 events
        for(count in 1:100){
            generateEvent()
            
        }
        avgbookshape <- bookShape(20)/NumEvents

        for(count in 2:NumEvents){
            generateEvent()
            avgbookshape<-avgbookshape+bookShape(20)/NumEvents
        }
        AvgBookShapes<- c(AvgBookShapes,avgbookshape)
    }
    
    # Fill the data of 4 sets to a matrix
    # The columns of the matrix corresponding to the sets of parameters (inputs)
    res<-matrix(AvgBookShapes,nrow=length(avgbookshape),ncol = 4, byrow = FALSE)
    
    return(res)

}

# Create a list contains the parameter: mu, and delta (alpha are the same for all 4 sets)
alphaList <- c(1,1,1,1)
muList<- c(10,8,10,10)
deltaList<- c(1/5, 1/5,1/6,1/8)

# Get the data of order books of 4 sets
# Number of events
Num=100
data<-orderSets(alphaList,muList,deltaList,Num)

# Plot the shapes of order books in one window by using par function
par(mfrow = c(2,2)) # combine the 4 plots in a 2 by 2 window filled by the row
set<- c('I','II','III','IV')
colors<- c("red","blue","orange","green")
for(i in 1:length(alphaList)){
    plot(-20:20,data[,i],main=paste("Parameters set:",set[i]),xlab="Relative price",ylab="# Shares", col=colors[i], type="b")

}

