# Load libraries
library(dplyr)

# Function to calculate the tax on capital gains based on capital gains, income,
# duration of investment and whether or not the filing is joint
cg_tax <- function(filing_status, income, investment, sale, time){
  # Initialize tax variable for tax paid on capital gains
  taxes <- NULL
  tax_rate <- NULL
  
  # Calculate gains from sale and investment
  gains <- max(sale - investment, 0)
  
  # Determine tax rate based on income for individuals
  if (filing_status=="Single filer"){
    if (income >= 0 & income <= 9525){
      tax_rate <- 0.1
    } else if (income > 9525 & income <= 38700){
      tax_rate <- 0.12
    } else if (income > 38700 & income <= 82500){
      tax_rate <- 0.22
    } else if (income > 82500 & income <= 157500){
      tax_rate <- 0.24
    } else if (income > 157500 & income <= 200000){
      tax_rate <- 0.32
    } else if (income > 200000 & income <= 500000){
      tax_rate <- 0.35
    } else if (income > 500000){
      tax_rate <- 0.37
    }
  } else if (filing_status == "Married, filing jointly") {
    # Determine tax rate based on income for joint filers
    if (income >= 0 & income <= 19050){
      tax_rate <- 0.1
    } else if (income > 19050 & income <= 77400){
      tax_rate <- 0.12
    } else if (income > 77400 & income <= 165000){
      tax_rate <- 0.22
    } else if (income > 165000 & income <= 315000){
      tax_rate <- 0.24
    } else if (income > 315000 & income <= 400000){
      tax_rate <- 0.32
    } else if (income > 400000 & income <= 600000){
      tax_rate <- 0.35
    } else if (income > 600000){
      tax_rate <- 0.37
    }
  } else if (filing_status == "Married, filing separately"){
    # Determine tax rate based on income for married but filing separately
    if (income >= 0 & income <= 9525){
      tax_rate <- 0.1
    } else if (income > 9525 & income <= 38700){
      tax_rate <- 0.12
    } else if (income > 38700 & income <= 82500){
      tax_rate <- 0.22
    } else if (income > 82500 & income <= 157500){
      tax_rate <- 0.24
    } else if (income > 157500 & income <= 200000){
      tax_rate <- 0.32
    } else if (income > 200000 & income <= 300000){
      tax_rate <- 0.35
    } else if (income > 300000){
      tax_rate <- 0.37
    }
  } else if (filing_status == "Head of Household"){
    # Determine tax rate based on income for head of households
    if (income >= 0 & income <= 13600){
      tax_rate <- 0.1
    } else if (income > 13600 & income <= 38700){
      tax_rate <- 0.12
    } else if (income > 38700 & income <= 82500){
      tax_rate <- 0.22
    } else if (income > 82500 & income <= 157500){
      tax_rate <- 0.24
    } else if (income > 157500 & income <= 200000){
      tax_rate <- 0.32
    } else if (income > 200000 & income <= 500000){
      tax_rate <- 0.35
    } else if (income > 500000){
      tax_rate <- 0.37
    }
  }
  # Calculate taxes on short-term and long-term capital gains
  if (time < 1){
    taxes <- gains*tax_rate
  } else if (time >= 1) {
    if (filing_status == "Single filer"){
      # Calculate long-term capital gains taxes for individuals
      if (income >= 0 & income <= 38600){
        taxes <- gains*0
      } else if (income > 38600 & income <= 425800){
        taxes <- gains*.15
      } else if (income > 425800){
        taxes <- gains*.2
      }
      # Add 3.8% net investment tax for large capital gains  
      if (gains > 200000){
        taxes <- taxes + gains*.038
      }  
    } else if (filing_status == "Married, filing jointly"){
        # Calculate long-term capital gains for joint filers
      if (income >= 0 & income <= 77200){
        taxes <- gains*0
      } else if (income > 77200 & income <= 479000){
        taxes <- gains*.15
      } else if (income > 479000){
        taxes <- gains*.2
      }
      # Add 3.8% net investment tax for large capital gains  
      if (gains > 250000){
        taxes <- taxes + gains*.038
      }
    } else if (filing_status == "Married, filing separately"){
        # Calculate long-term capital gains for separate but married
      if (income >= 0 & income <= 38600){
        taxes <- gains*0
      } else if (income > 38600 & income <= 239500){
        taxes <- gains*.15
      } else if (income > 239500){
        taxes <- gains*.2
      }
      # Add 3.8% net investment tax for large capital gains  
      if (gains > 125000){
        taxes <- taxes + gains*.038
      }
    } else if (filing_status == "Head of Household"){
      # Calculate long-term capital gains for head of household individuals
      if (income >= 0 & income <= 51700){
        taxes <- gains*0
      } else if (income > 51700 & income <= 452400){
        taxes <- gains*.15
      } else if (income > 452400){
        taxes <- gains*.2
      }
      # Add 3.8% net investment tax for large capital gains  
      if (gains > 200000){
        taxes <- taxes + gains*.038
      }
    }
  }
  # Output taxes on capital gains
  return(taxes)
}

# Function to calculate the taxes on capital gains re-invested in QOF's based on duration of investment
qof_tax <- function(filing_status, income, investment, sale, time){
  # Calculate taxes on gains from investment in QOF
  if (time < 5){
    basis <- investment
  } else if (time >= 5 & time < 7){
    basis <- investment*1.1
  } else if (time >= 7 & time < 10){
    basis <- investment*1.15
  } else if (time >= 10){
    basis <- sale
  }
  # Calculate taxes from capital gains with adjusted basis for QOF's
  taxes <- cg_tax(filing_status, income, basis, sale, time)
  # Output taxes on capital gains in QOF
  return(taxes)
}

