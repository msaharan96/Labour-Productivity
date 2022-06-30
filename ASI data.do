clear all
cd "/Users/msaharan96/Library/CloudStorage/OneDrive-NATIONALINSTITUTEOFINDUSTRIALENGINEERING/Project 'Labour Productivity in Indian Industry'/Data/ASI"

import excel "ASI.xlsx", sheet("Data") firstrow
drop FYear D AC NICCode Name DigitConcordanceSerieshasb
gen year = real(Year)
encode Measure, gen(measure)
order year measure
drop Measure Year

import excel "ASI.xlsx", sheet("T Data") cellrange(A3:HB27) firstrow clear
reshape long FixedCapital WagesandSalariesWorkers NumberofEmployees NumberofWorkers TotalInput GrossValueAdded MaterialsConsumed Depreciation NetValueAdded TotalPersonsEngaged NumberofFactories, i(A)
ren (A _j) (sector year)
encode sector, gen(Sector)
order Sector year
export excel year sector FixedCapital WagesandSalariesWorkers NumberofEmployees NumberofWorkers TotalInput GrossValueAdded MaterialsConsumed Depreciation NetValueAdded TotalPersonsEngaged NumberofFactories using "ASI Final", sheetreplace firstrow(variables)


