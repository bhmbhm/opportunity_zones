# Load libraries
library(dplyr)

# Function to calculate the tax on capital gains based on capital gains, income,
# duration of investment and whether or not the filing is joint

# Income brackets and tax calculations based off of the following resources:
# https://www.irs.gov/pub/irs-drop/rp-18-57.pdf
# https://www.irs.gov/taxtopics/tc409
# https://www.irs.gov/taxtopics/tc559

cg_tax <- function(filing_status, income, investment, sale, time){
  # Initialize tax variable for tax paid on capital gains
  taxes <- NULL
  tax_rate <- NULL
  
  # Calculate gains from sale and investment
  gains <- max(sale - investment, 0)
  
  # Determine tax rate based on income for individuals
  if (filing_status=="Single filer"){
    if (income >= 0 & income <= 9700){
      tax_rate <- 0.1
    } else if (income > 9700 & income <= 39475){
      tax_rate <- 0.12
    } else if (income > 39475 & income <= 84200){
      tax_rate <- 0.22
    } else if (income > 84200 & income <= 160725){
      tax_rate <- 0.24
    } else if (income > 160725 & income <= 204100){
      tax_rate <- 0.32
    } else if (income > 204100 & income <= 510300){
      tax_rate <- 0.35
    } else if (income > 510300){
      tax_rate <- 0.37
    }
  } else if (filing_status == "Married, filing jointly") {
    # Determine tax rate based on income for joint filers
    if (income >= 0 & income <= 19400){
      tax_rate <- 0.1
    } else if (income > 19400 & income <= 78950){
      tax_rate <- 0.12
    } else if (income > 78950 & income <= 168400){
      tax_rate <- 0.22
    } else if (income > 168400 & income <= 321450){
      tax_rate <- 0.24
    } else if (income > 321450 & income <= 408200){
      tax_rate <- 0.32
    } else if (income > 408200 & income <= 612350){
      tax_rate <- 0.35
    } else if (income > 612350){
      tax_rate <- 0.37
    }
  } else if (filing_status == "Married, filing separately"){
    # Determine tax rate based on income for married but filing separately
    if (income >= 0 & income <= 9700){
      tax_rate <- 0.1
    } else if (income > 9700 & income <= 39475){
      tax_rate <- 0.12
    } else if (income > 39475 & income <= 84200){
      tax_rate <- 0.22
    } else if (income > 84200 & income <= 160725){
      tax_rate <- 0.24
    } else if (income > 160725 & income <= 204100){
      tax_rate <- 0.32
    } else if (income > 204100 & income <= 306175){
      tax_rate <- 0.35
    } else if (income > 306175){
      tax_rate <- 0.37
    }
  } else if (filing_status == "Head of Household"){
    # Determine tax rate based on income for head of households
    if (income >= 0 & income <= 13850){
      tax_rate <- 0.1
    } else if (income > 13850 & income <= 52850){
      tax_rate <- 0.12
    } else if (income > 52850 & income <= 84200){
      tax_rate <- 0.22
    } else if (income > 84200 & income <= 160700){
      tax_rate <- 0.24
    } else if (income > 160700 & income <= 204100){
      tax_rate <- 0.32
    } else if (income > 204100 & income <= 510300){
      tax_rate <- 0.35
    } else if (income > 510300){
      tax_rate <- 0.37
    }
  }
  # Calculate taxes on short-term and long-term capital gains
  if (time < 1){
    taxes <- gains*tax_rate
  } else if (time >= 1) {
    if (filing_status == "Single filer"){
      # Calculate long-term capital gains taxes for individuals
      if (income >= 0 & income <= 39475){
        taxes <- gains*0
      } else if (income > 39475 & income <= 510300){
        taxes <- gains*.15
      } else if (income > 510300){
        taxes <- gains*.2
      }
      # Add 3.8% net investment tax for large capital gains  
      if (gains > 200000){
        taxes <- taxes + gains*.038
      }  
    } else if (filing_status == "Married, filing jointly"){
        # Calculate long-term capital gains for joint filers
      if (income >= 0 & income <= 78950){
        taxes <- gains*0
      } else if (income > 78950 & income <= 612350){
        taxes <- gains*.15
      } else if (income > 612350){
        taxes <- gains*.2
      }
      # Add 3.8% net investment tax for large capital gains  
      if (gains > 250000){
        taxes <- taxes + gains*.038
      }
    } else if (filing_status == "Married, filing separately"){
        # Calculate long-term capital gains for separate but married
      if (income >= 0 & income <= 39475){
        taxes <- gains*0
      } else if (income > 39475 & income <= 306175){
        taxes <- gains*.15
      } else if (income > 306175){
        taxes <- gains*.2
      }
      # Add 3.8% net investment tax for large capital gains  
      if (gains > 125000){
        taxes <- taxes + gains*.038
      }
    } else if (filing_status == "Head of Household"){
      # Calculate long-term capital gains for head of household individuals
      if (income >= 0 & income <= 52850){
        taxes <- gains*0
      } else if (income > 52850 & income <= 510300){
        taxes <- gains*.15
      } else if (income > 510300){
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

