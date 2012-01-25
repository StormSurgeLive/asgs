/*
 * This file is part of AcToShape
 * by Mark Phillips
 * mphillip@unca.edu
 *
 * Copyright (c) 2009  University of North Carolina at Asheville
 * Licensed under the RENCI Open Source Software License v. 1.0.
 * See the file LICENSE.txt for details.
 */

/**
 * Vertex stores information associated with a vertex (node) in an
 * ADCIRC triangle mesh, including the latitude (x), longitude (y),
 * and depth.  The Vertex class also contains arrays for storing the
 * computed water levels at the vertex at each time step of a
 * simulation.
 */
package edu.unca.nemac.gis;

class Vertex {
    /**
     * The vertex longitude.
     */
    public double x;

    /**
     * The vertex latitude.
     */
    public double y;

    /**
     * The depth of the land surface below MSL (mean sea level) at this node.
     * Note that ADCIRC uses bathymetry data, which is the negative of elevation data:
     * Positive values represent points below sea level, and negative values
     * are points above sea level.
     */
    public double depth;

    /**
     * Array of computed water levels for this vertex from the ADCIRC .63 file.
     * This array has one value for each timestep in a simulation run.
     */
    public double level63[];

    /**
     * Number of this vertex in the grid file.
     */
    public int index;

    /**
     * Array of "flood water levels" for this vertex.  The "flood water
     * level" represents the total height of water above the surface
     * of the land at this vertex.  Positive flood water level values
     * indicate that there is water above the land surface at this
     * vertex.  These values are computed by adding the vertex's depth
     * value to its level63 value, and then clamping the resulting
     * value to 0 if the result was negative.  In other words:
     * <pre>
     *      floodlevel[i] = level63[i] + depth;
     *      if (floodlevel[i] < 0) { floodlevel[i] = 0; }
     * </pre>
     * Floodlevel is essentially innundation level --- the depth of
     * the water over the land at a point.  Floodlevel is not just set
     * for vertices that are normally on dry land, though.  It is set
     * for ALL vertices.  For vertices that are normally below MSL
     * (under water), floodlevel gives the (simulated) depth of the
     * water at that point.  Floodlevel is never negative.  A floodlevel
     * of 0 means that the land is dry at the vertex.
     */
    public double floodlevel[];

    public Vertex(int index, double x, double y, double depth) {
	this.index = index;
        this.x     = x;
        this.y     = y;
        this.depth = depth;
    }

    public static Vertex average(Vertex a, Vertex b) {
        Vertex c = new Vertex( -1, (a.x + b.x)/2, (a.y + b.y)/2,  (a.depth + b.depth)/2);
        if (a.level63 != null && b.level63 != null) {
            c.level63  = new double[a.level63.length];
            c.floodlevel = new double[a.level63.length];
            for (int i=0; i<c.level63.length; ++i) {
                c.level63[i] = (a.level63[i] + b.level63[i])/2;
                c.floodlevel[i] = c.level63[i] + c.depth;
                if (c.floodlevel[i] < 0) { c.floodlevel[i] = 0; }
            }
        }
        return c;
    }

}
