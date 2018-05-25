from datetime import datetime
from pptx import Presentation
from pptx.util import Inches


# Read parsed run.properties
f = open('cpraHydro.info','r')
info = f.readlines()
f.close()
storm=info[0].strip()
grid=info[1].strip()
advisoryTime=info[2].strip()
coldStartTime=info[3].strip()
advisory=info[4].strip()

# Convert advisoryTime to python datetime object
advisory_dt = datetime.strptime(advisoryTime,'%Y%m%d%H%M%S')
advisory_dt_long = datetime.strftime(advisory_dt,'%b-%d-%Y %H:%M')

prs = Presentation('LSU_template.pptx')

slide_layout = prs.slide_layouts[5]
slide_layout_hydro = prs.slide_layouts[6]

# Create a title slide
title_slide_layout = prs.slide_layouts[0]
slide = prs.slides.add_slide(title_slide_layout)
title = slide.shapes.title
subtitle = slide.placeholders[1]
#title.text = "Hurricane Isaac"
title.text = "STORM " + storm
#subtitle.text = "Advisory #25 - Issued on 09:00 UTC Aug-27-2012"
subtitle.text = "Advisory #" + advisory + " Issued on " + advisory_dt_long + " UTC"

# Set slide layout
left = Inches(1.94)
top = Inches(1.14)

#img_path = 'LA_v17a_09_25_nhcOfficial_maxele_0001.jpg'
#slide = prs.slides.add_slide(slide_layout)
#title = slide.shapes.title
#subtitle = slide.placeholders[1]
#title.text = "NHC Advisory 25 Consensus"
#subtitle.text = "Simulated maximum water level (ft, NAVD88)"
#pic = slide.shapes.add_picture(img_path,left,top)
#
#img_path = 'LA_v17a_09_25_veerRight50_maxele_0001.jpg'
#slide = prs.slides.add_slide(slide_layout)
#title = slide.shapes.title
#subtitle = slide.placeholders[1]
#title.text = "NHC Advisory 25 veerRight50"
#subtitle.text = "Simulated maximum water level (ft, NAVD88)"
#pic = slide.shapes.add_picture(img_path,left,top)

left = Inches(0.42)
top = Inches(1.15)

fnames = ['WSE_17StCanal_USACE85625.png','WSE_IHNC01_USACE76065.png','WSE_IHNC02_USACE76030.png',
        'WSE_LPV144_USACE76010.png','WSE_LPV149_USACE85760.png','WSE_NOV13_USACE01440.png',
        'WSE_NOV14_USACE01440.png','WSE_WBV09a_USACE82770.png','WSE_WBV09b_USACE82762.png',
        'WSE_WBV162_USACE82742.png','WSE_WBV7274_USACE82715.png','WSE_WBV90_USACE76265.png']

# Station names correspond to the order of fnames
staName = ['17th St. Outfall Canal','Seabrook Complex (IHNC-01)','IHNC Surge Barrier (IHNC-02)',
        'Bayou Dupre Sector Gate (LPV-144)','Caernarvon Canal Sector Gate (LPV-149)',
        'Empire Floodgate (NOV-13)','Empire Lock (NOV-14)','Oakville Sluice Gate (WBV-09a)',
        'Hero Canal stop-log gage (WBV-09b)','Bayou Segnetee closure (WBV-16.2)',
        'Western Tie-In features (WBV-74-72)','West Closure Complex (WBV-90)']

i = 0
for image in fnames:
    slide = prs.slides.add_slide(slide_layout_hydro)
    title = slide.shapes.title
    title.text = staName[i]
    pic = slide.shapes.add_picture(image,left,top)
    i = i + 1


# Add image to slide
#img_path = '1_WSE_85625.png'
#slide = prs.slides.add_slide(slide_layout_hydro)
#title = slide.shapes.title
#title.text = "17th St. Outfall Canal"
#pic = slide.shapes.add_picture(img_path,left,top)
#
#img_path = '2_WSE_76065.png'
#slide = prs.slides.add_slide(slide_layout_hydro)
#title = slide.shapes.title
#title.text = "Seabrook Complex (IHNC-01)"
#pic = slide.shapes.add_picture(img_path,left,top)


#prs.save('Isaac_Adv25_082720180900.pptx')
pptFile = storm + "_Adv" + advisory + "_" + advisoryTime + ".pptx"
#prs.save('Isaac_Adv25_082720180900.pptx')
prs.save(pptFile)
