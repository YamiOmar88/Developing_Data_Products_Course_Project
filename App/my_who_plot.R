my_who_plot <- function(data, cols, nationality){
        
        # Read the list of countries with region categories
        CountriesList <- read.csv("./data/CountriesList.csv")
        
        # Get the rows that match the nationality
        row <- grep(nationality, CountriesList$country_name, ignore.case = TRUE)
        
        # Check the length(row) and find the region
        library(stringdist)
        
        if(length(row) == 1){
                region <- CountriesList$sub_region_name[row]
        } else {
                region <- CountriesList$sub_region_name[row]
                if(length(unique(region)) == 1){
                        region <- unique(region)
                } else {
                        bestmatch <- which.min(stringdist(nationality, CountriesList$country_name))
                        region <- CountriesList$sub_region_name[bestmatch]
                }
        }
        
        # Get the list of countries belonging to that region
        library(dplyr)
        countries <- CountriesList %>% 
                                filter(sub_region_name == region) %>%
                                select(country_name)
        countries <- countries$country_name
        
        aux <- vector(mode = "integer", length = 0)
        for(i in 1:length(countries)){
                aux[i] <- which.min(stringdist(countries[i], cols))
        }
        mydata <- as.matrix(data[19:25 , aux])
        
        
        # Prepare the data to return a plot
        x <- c("Cereals", "Fruits, vegetables, Pulses, Nuts", 
               "Meat, Fish, Milk, Eggs", "Oils, Fats, Sugars", "Others",
               "Roots, Tubers", "Total Calories/day")
        y <- apply(mydata, 1, mean, na.rm = TRUE)
        sd <- apply(mydata, 1, sd, na.rm = TRUE)
        xy <- data.frame(x = x, y = y, sd = sd)
        
        n <- ncol(mydata)
        ymax <- y + qt(0.95, n - 1) * sd / sqrt(n)
        ymin <- y - qt(0.95, n - 1) * sd / sqrt(n)
        
        xy$se1 <- ymax - xy$y
        xy$se2 <- xy$y - ymin
        
        mylabel <- paste("Total = ", round(xy$y[7], 0), "Calories/day")

        # Make plot
        library(ggplot2)
        p <- ggplot(xy[1:6, ], aes(x = factor(x), y = y)) 
        p <- p + geom_bar(stat = "identity", colour = "black", fill = "pink")
        p <- p + geom_errorbar(aes(ymax = y + se1, ymin = y - se2, width = 0.25))
        p <- p + xlab("") + ylab("Calories/day")
        p <- p + coord_flip()
        p <- p + ggtitle(label = mylabel)
        return(p)
}