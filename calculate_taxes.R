# Load libraries
library(dplyr)

# Create tax bracket named lists to store values
# https://www.nerdwallet.com/blog/taxes/federal-income-tax-brackets/
ind_tax_rates <- c(0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37)
names(ind_tax_rates) <- c("$0-$9,525", "$9,526-$38,700", "$38,701-$82,500", "$82,501-$157,500", "$157,501-$200,000", "$200,001-$500,000", ">$500,001")

joint_tax_rates <- c(0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37)
names(joint_tax_rates) <- c("$0-$19,050", "$19,051-$77,400", "$77,401-$165,000", "$165,001-$315,000", "$315,001-$400,000", "$400,001-$600,000", ">$600,001")

# Function to calculate the tax on capital gains based on capital gains, income,
# duration of investment and whether or not the filing is joint
cg_tax <- function(gains, income_bracket, time, joint){
  # Determine tax rate
  if (joint==TRUE){
    tax_rate <- joint_tax_rates[income_bracket]
  } else if (joint==FALSE) {
    tax_rate <- ind_tax_rates[income_bracket]
  }
  # Calculate taxes on short-term and long-term capital gains
  if (time < 1){
    taxes <- gains*tax_rate
  } else if (time >= 1) {
    if (joint == FALSE){
      # Calculate long-term capital gains for individuals
      if (which(income_bracket == names(ind_tax_rates)) < 3){
        taxes <- gains*0
      } else if (which(income_bracket == names(ind_tax_rates)) >= 3 & which(income_bracket == names(ind_tax_rates)) < 7){
        taxes <- gains*.15
      } else if (which(income_bracket == names(ind_tax_rates)) == 7){
        taxes <- gains*.2
      }
      # Add 3.8% net investment tax for large capital gains  
      if (gains >200000){
        taxes <- taxes + gains*.038
      }  
    } else if (joint == TRUE) {
        # Calculate long-term capital gains for joint
      if (which(income_bracket == names(joint_tax_rates)) < 3){
        taxes <- gains*0
      } else if (which(income_bracket == names(joint_tax_rates)) >= 3 & which(income_bracket == names(joint_tax_rates)) < 7){
        taxes <- gains*.15
      } else if (which(income_bracket == names(joint_tax_rates)) == 7){
        taxes <- gains*.2
      }
      # Add 3.8% net investment tax for large capital gains  
      if (gains >250000){
        taxes <- taxes + gains*.038
      }
    }
  }
  # Output taxes on capital gains
  return(taxes)
}

# Function to calculate the taxes on capital gains re-invested in QOF's based on duration of investment
qof_tax <- function(gains, income_bracket, time, basis, joint=FALSE){
  # Calculate taxes on gains from investment in QOF
  if (time < 5){
    new_basis <- basis 
  } else if (time >= 5 & time < 7){
    new_basis <- basis*1.1
  } else if (time >= 7 & time < 10){
    new_basis <- basis*1.15
  } else if (time >= 10){
    new_basis <- basis + gains
  }
  # Calculate taxes from capital gains with adjusted basis for QOF's
  taxable_income <- (basis + gains) - new_basis
  taxes <- cg_tax(taxable_income, income_bracket, time, joint)
  # Output taxes on capital gains in QOF
  return(taxes)
}



