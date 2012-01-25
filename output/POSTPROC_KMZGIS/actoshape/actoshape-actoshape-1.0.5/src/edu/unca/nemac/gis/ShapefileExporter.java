/*
 * This file is part of AcToShape
 * by Mark Phillips
 * mphillip@unca.edu
 *
 * Copyright (c) 2009  University of North Carolina at Asheville
 * Licensed under the RENCI Open Source Software License v. 1.0.
 * See the file LICENSE.txt for details.
 */

package edu.unca.nemac.gis;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.geotools.data.FeatureWriter;
import org.geotools.data.Transaction;
import org.geotools.data.shapefile.ShapefileDataStore;
import org.geotools.feature.Feature;
import org.geotools.feature.FeatureType;


public class ShapefileExporter {

    private File outfile;
    private String outfileBase;
    private FeatureWriter fw;
    private FeatureType featureType;
    private String prj = WGS84_ESRI_PRJ;
    private boolean firstTime = true;

    public final static String WGS84_ESRI_PRJ = "GEOGCS[\"GCS_WGS_1984\"," +
	"DATUM[\"D_WGS_1984\"," +
	"SPHEROID[\"WGS_1984\",6378137.0,298.257223563]]," +
	"PRIMEM[\"Greenwich\",0.0]," +
	"UNIT[\"Degree\",0.0174532925199433]]";

    public ShapefileExporter(String outfileName) throws IOException {
	if (outfileName.endsWith(".shp")) {
	    this.outfileBase = outfileName.substring(0, outfileName.length() - 4);
	} else {
	    this.outfileBase = outfileName;
	}
        this.prj = WGS84_ESRI_PRJ;
    }


    private void writePrj() throws IOException {
        BufferedWriter bw = new BufferedWriter(new FileWriter(outfileBase+".prj"));
        bw.write(prj);
        bw.flush();
        bw.close();

    }
    
    private void shpInit() throws IOException {
	this.outfile = new File(outfileBase + ".shp");
        ShapefileDataStore store = new ShapefileDataStore(outfile.toURI().toURL());
        store.createSchema(featureType);
        fw = store.getFeatureWriter(store.getTypeNames()[0], Transaction.AUTO_COMMIT);
    }


    public void addFeature(Feature feature) throws Exception {
	if (firstTime) {
	    // Extract featureType from first feature
	    this.featureType = feature.getFeatureType();
	    shpInit();
	    firstTime = false;
	}

	// Ignore and return if this feature type does not match
	if (! feature.getFeatureType().equals(featureType)) {
	    return;
	}

	Object[] att = null;
	Feature writeFeature = fw.next();
	att = feature.getAttributes(att);
	for (int n=0; n<att.length; n++) {
	    writeFeature.setAttribute(n, att[n]);  
	}
	fw.write();
    }



    public void close() throws Exception {
	if (fw != null) {
	    fw.close();
	    writePrj();
	}
    }


    /**
     * Just in case we forgot to close...
     */
    public void finalize() {
        try {
            close();
        } catch (Exception e) {
            //e.printStackTrace();
        }
    }

}


