# GenUnits
Basic units of measure library enabling calculation with units

# Background

Calculating values that have an unit of measure, like mass, time, volume etc.. is inherintly difficult when different units are involved. This specifically applies to the field of medical prescription calculations. For example calculating a drip rate can be awkward, like:

> 2 mL of dopamine 200 mg / 5 mL in 48 mL saline infused at a driprate of 2 mL/hour equals how ... mcg/kg/min </br>
> calculated for a body weight of 10 kg.

The concentration of dopamine is 40 mg / mL dissolved in 50 mL gives a concentration of 0.8 mg/mL, infused at a rate of 2 mL/hour equals 1.6 mg /hour, which is 0.16 mg/kg/hour or 160 mcg/kg/hour, with 60 minutes in an hour the resulting dose equals 2.67 mcg/kg/hour. To complicate matters, different medical settings request showing the same calculations in different units within the same unit group. For example, in some hospital morfine dose is measured in mg/kg/day, while in other hospitals mg/kg/hour is prefered.

This library aims to allow for direct calculations with values and units. For example, when 2 mL/hour is multiplied with 1 hour to calulate administered fluid, GenUnits evaluates 2 mL/hour * 2 hour as 4 mL. It also provides the alternative unit options. So for mg/day, the same value can be shown as mcg/min or g/week or mg/hour.

# Libray design
This repository uses an explicit opt-in `.gignore` strategy, meaning that all files are excluded unless specifically included via the `.gitignore` file.


