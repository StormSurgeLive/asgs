% ASGS Road Map 2014 
% by Jason Fleming, Seahorse Coastal Consulting
% 14 November 2013

<!--  
~/.cabal/bin/pandoc -o ASGSRoadMap2014.pdf --variable mainfont=Georgia --latex-engine=xelatex --variable sansfont=Arial --variable fontsize=12pt --variable geometry:margin=1in --number-sections --toc ASGSRoadMap2014.md
-->

<!--
~/.cabal/bin/pandoc -o ASGSRoadMap2014.html --variable mainfont=Georgia --variable sansfont=Arial --number-sections --toc ASGSRoadMap2014.md
-->

Executive Summary
=================

Each year, the "off season" (December through May) gives us a time 
to reflect upon our performance in the previous hurricane season as 
well as plan and implement new features and capabilities. This 
document serves as an inventory of planned improvements that allows 
all Teams to coordinate their planning, implementation and testing
processes. 

Introduction
============

Each Team section below briefly describes the vision and/or goals 
the corresponding Team has for the technological and organizational 
infrastructure that they plan to have in place on 1 June 2013, as 
well as the resultant list of tasks to be accomplished in the 
intervening period.  

All Teams
=========

There are some events or action items that pertain to all Teams; these
items are detailed in this section. 

Testing
-------

Real time collaboration on high profile threats gives us an 
opportunity to achieve either a spectacular success or notorious 
failure; unfortunately the difference between these outcomes may 
come down to having a semicolon in the wrong place among tens of 
thousands of lines of code. As a result, end-to-end testing is a 
crucial element of our strategy.

In keeping with this, I would like to formally set a date for our 
collective burn-in tests (drills) for the 2014 season, rather than 
relying on the opportunistic approach that we have used in the past. 
Furthermore, I think we need two separate drills in the preseason, 
since these tests often expose serious shortcomings or other issues 
that require extensive adjustments. The second drill is needed to 
confirm that we've addressed the issues uncovered by the first one. 
Here is my proposed timetable:

* Beta test: May 1--2 2014
* Release test: May 29--30 2014

Clients/Stakeholders
--------------------

I'd like to compile a list of regional clients or stakeholders for each 
Team, as well as a list of national agencies that use our guidance. The
lists will include local weather forecast offices, emergency managers,
river forecast offices, public officials, etc.

North Carolina
==============

OU/NSSL hydrology modeling.

Louisiana
=========

New Orleans District
====================

Texas
=====

NOAA CSDL
=========

Seahorse
========

Branching the 2013 ASGS; starting development of 2014 ASGS.

Moving to ASGS to github.  

Documentation
-------------

ASGS Interface guide (2014). ASGS Developers Guide. ASGS Operators Guide. 
ASGS Tutorial. 

Capabilities
------------

Web based graphical user interface for the ASGS, including configuration,
control, and monitoring. 

Communication
-------------

New website. Will contain all ASGS documentation, information about the
real time ASGS participants. Will host the ASGS config/control app. 

ASGS Up and Running training session at ADCIRC Boot Camp.

Recruiting
----------

FL, GA, SC, DE/MD/VA, NY/NJ.  




