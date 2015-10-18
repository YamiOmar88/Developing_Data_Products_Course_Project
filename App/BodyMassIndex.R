# This function takes the weight in kg and the height in m and returns the BMI
# and the classification for the subject.

BodyMassIndex <- function(weight, height){
        bmi <- weight/height^2
        
        if(bmi < 15){classif <- "very severely underweight"
        }else if(bmi > 15 & bmi < 16){classif <- "severely underweight"
        }else if(bmi > 16 & bmi < 18.5) {classif <- "underweight"
        }else if(bmi > 18.5 & bmi < 25) {classif <- "healthy weight"
        }else if(bmi > 25 & bmi < 30) {classif <- "overweight"
        }else if(bmi > 30 & bmi < 35) {classif <- "moderately obese"
        }else if(bmi > 35 & bmi < 40) {classif <- "severely obese"
        }else if(bmi > 40) {classif <- "very severely obese"}
        
        ans <- data.frame(BMI = round(bmi, 1), category = classif)
        return(ans)
}