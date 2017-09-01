module AhuText exposing (ahutext)


ahutext : String
ahutext =
    """
Volume 3 No.2, Spring 1999
ISSN# 1523-9926

# Simulating the performance of an Air Handling Unit on a Psychrometric Chart

By

David B. Meredith <dxm15@psu.edu>
Mark W Meredith <mwm126@gmail.com>

School of Engineering Technology Department of Engineering Science
and Commonwealth Engineering The Pennsylvania State University
The Pennsylvania State University

# Abstract

When teaching the principles of Heating, Ventilating and Air-conditioning, it is
usually difficult to get students to visualize the interaction among all of the
inter-dependent variables as they pass through an Air Handling Unit (AHU).
Similarly, it is difficult to explain how these processes would appear on a
psychrometric chart. The Web-based software package (JAVA) was developed to
assist faculty who teach in these disciplines, and to demonstrate to students
the various functions and relationships involved.


# Introduction

There are three basic fields to this page. In the upper left corner is an
animated and interactive model of the airflow through the AHU. The right side of
the page shows an animated and interactive psychrometric chart including a
sensible heat factor protractor. The bottom left corner contains an interactive
control panel for adjusting parameters in the simulation. Each of these sections
will be discussed in detail below.


# The Air Handling Unit

The user can observe the representative air molecule as it passes through the
unit . As the air molecule passes through this loop, its color changes to
represent the air temperature on a color spectrum scale (red is relatively hot
and blue is relatively cool). The flow rate and fraction of outside air can be
adjusted by the user.

A fraction of the conditioned return air is exhausted to the outside (EA) and
replaced with outside air (OA). This flow of fresh air into the building is
required to maintain good indoor air quality (IAQ), but the quantity of outside
air (as a percentage of the total airflow) can be adjusted by the user. The
mixed air (MA) then passes through the cooling coil where both sensible energy
(temperature) and latent energy (moisture) are removed from the air stream to
become supply air (SA). The cooled and dehumidified air is then ducted into the
building to absorb energy and moisture from the thermal load.

# The Psychrometric Chart

The right side of the page displays the animated and interactive psychrometric
chart. The horizontal axis represents dry bulb temperature (sensible energy) and
the vertical axis represents absolute humidity ratio (latent energy). The
saturation line is also indicated on this chart. Superimposed on this chart is a
rectangle representing the nominal comfort zone used in the HVAC&R industry. The
boundaries of this zone range from 70 to 80F on the horizontal scale and from
0.005 to 0.010 pounds of water vapor per pound of dry air on the vertical axis.
Also functioning in the psychrometric chart window is the Sensible Heat Factor
(SHF) protractor with two animated vectors. One vector represents the cooling
load required by the building and the other vector represents the cooling
supplied by the cooling system. Equilibrium exists when these two vectors are
concentric.


# Control Panel and Limitations

The lower left window on the screen contains the control panel where the user
can modify various parameters. These are grouped into several related functions.
The first three parameters are usually controlled by the building mechanical
manager. The second group represent the outside air conditions, and the third
group represent the building load parameters. The bottom line of the control
panel allows the user to adjust the Simulation Speed. The speed can be varied
from very slow (to watch the processes occur simultaneously in the AHU and on
the psychrometric chart) to fast (to observe how changes are reflected in the
location of the room air within the comfort zone).

Changing the Percent of Outside Air moves the location of the Mixed Air
condition on the psychrometric chart, and also affects the fraction of the air
molecule in the AHU that exits as exhaust air and returns as supply air. While
changing this parameter can have a major effect on the thermal load on the
cooling coil and the overall cost of cooling a building, this program is not
designed to address those issues. The Supply Air flow rate can be adjusted to
match the load requirements. This action demonstrates the use of Variable Air
Volume (VAV) systems commonly found in commercial projects today. The second
method of modifying the cooling rate is to change the Supply Air Temperature
(commonly referred to as reset). The interaction between these two parameters
has strong implications for the operating cost of the overall system, but that
issue is beyond the scope of this project.

The Outside Air Temperature and the outside Wet Bulb Temperature represent the
ambient weather conditions. The primary effect in this simulation is to alter
the location of the OA point on the psychrometric chart. In a real sense, the
outside temperature affects the building load. However to simplify the modeling
of these processes, these two functions have not been coupled.

The building load is modeled as a Total Load (measured in Tons of refrigeration)
and a Sensible Heat Fraction. These two parameters define the length and slope
of the cooling load vector on the psychrometric chart's protractor. At
equilibrium, the same slope is used on the psychrometric chart between the
supply air condition and the return air condition. When not at equilibrium, the
slope between these points represents the cooling rate being supplied. The
difference between the cooling load and the cooling rate causes the room air
condition to migrate to a new equilibrium point.

One of the main strengths of this learning tool is to help students visualize
how room conditions are affected by changing building loads, and how the
building operator attempts to maintain design conditions within the space by
adjusting air flow and temperature.

In the message line at the bottom of the simulation, the user is flashed brief
messages when parameters are out of range. For example, if the room temperature
or humidity falls above or below reasonable preset limits, a message indicting
that the occupants are unhappy is shown. If the user tries to increase the air
flow above the nominal maximum of 40,000 cfm, they are warned about the noise
generated at high duct velocities. These messages are to remind the novice user
that the operation of a building mechanical system is even more complex than
what is represented by this simulation.


# Relationships and equations

The conditions for each state point of air are calculated from equations found
in Chapter 6 of the 1997 ASHRAE Handbook of Fundamentals, Atlanta, GA. These
equations relate the dry bulb temperature, wet bulb temperature, dew point
temperature, relative humidity, absolute humidity ratio and enthalpy.

The user defines the dry bulb temperature and wet bulb temperature of the
Outside Air via the control panel. The user also defines the Supply Air
temperature via the control panel, and the relative humidity of the Supply air
is assumed constant at 90%. The Mixed Air conditions are linearly interpolated
from the user-defined percent of Outside Air and by the conditions of the return
air and outside air. The return air conditions are allowed to float in response
to the parameter values selected by the user.

The conditions at each point (Supply Air, Return Air, Outside Air and Mixed Air)
are calculated at each time step. The animation of the points is linked to where
the air molecule is in the AHU model. The return air and outside air are simply
proportional movements along the process line between them. The process between
the mixed air condition and the supply air (i.e., through the cooling coil) is
modeled to follow the profile of an ellipse in the second quadrant. This path
demonstrates that mostly sensible cooling occurs in the first half of the coil
rows, followed by a combination of sensible cooling and dehumidification in the
last half of the coil rows. The process between the supply air condition and the
return air (or room air) follows the process line defined by the sensible heat
ratio in the protractor.

A simple energy and mass balance model is used to account for changes in the
condition of the room air. Constant sensible and latent capacitance values have
been assumed to make it easy for the user to observe changes but for the system
to return to equilibrium within a few minutes.

The cooling load of the building and cooling supply rates are determined using a
total load and Sensible Heat Factor (SHF) concept. The total load (Btu/hr) is
the sum of the sensible load and the latent load. The SHF is the ratio of the
sensible load to the total load. Typical office buildings are represented by an
SHF = 0.9. The loads are measured in Tons of refrigeration, where one Ton equals
12,000 Btu/hr. The flow rate of Supply air is measured in cubic feet per minute
(cfm).



# Conclusions

A web-based tool to assist in the study of building system operation has been
introduced. The simulation allows the student to control various parameters, and
to visualize the effect those parameter changes have on the operation of the
system. It also allows the student to compare what is happening in the physical
system represented by the building schematic with how the air conditions are
changing on the psychrometric chart. Finally, this software package allows the
students to grasp the time-dependent nature of building mechanical systems. By
acting as the building mechanical manager, they have the opportunity to try to
maintain acceptable comfort conditions in the safe (and inexpensive) environment
of a simulation. Finally, it should be noted that this simulation is for
educational purposes only, and should never be used as a design tool.

"""
