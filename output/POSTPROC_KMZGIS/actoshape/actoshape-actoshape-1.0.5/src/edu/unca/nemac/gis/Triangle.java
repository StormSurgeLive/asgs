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
 * Triangle represents a triangle in an ADCIRC triangle mesh.  It
 * includes methods for doing various useful computations with the
 * triangle.
 */

package edu.unca.nemac.gis;

class Triangle {
    /**
     * An array of 3 indices into a list of Vertices.  The vertex list
     * is not stored in the Triangle itself --- it is passed in as a parameter
     * to methods that need it.
     */
    public int vertex_indices[];

    /**
     * The triangle's average depth; this is obtained by averaging
     * the depths of its 3 vertices.
     */
    public double depth;

    /**
     * Array of average computed water levels for this triangle;
     * obtained by averaging the level63[] values from each vertex.
     */
    public double level63[];

    /**
     * Array of average flood water levels for this triangle;
     * obtained by averaging the floodlevel[] values from each vertex.
     */
    public double floodlevel[];


    /**
     * The number of this triangle in the ADCRIC grid file.
     */
    public int index;

    /**
     * Create a new triangle with the given vertex indices.
     */
    public Triangle(int index, int i, int j, int k) {
        this.index = index;
        vertex_indices = new int[3];
        vertex_indices[0] = i;
        vertex_indices[1] = j;
        vertex_indices[2] = k;
    }

    /**
     * Compute this Triangle's depth and level values.
     *
     * @param vertices          array of vertices for the mesh
     * @param timesteps         number of timesteps in the simulation
     */
    public void compute(Vertex vertices[], int timesteps) {
        // compute average depth
        this.depth = 0;
        for (int i=0; i<3; ++i) {
            this.depth += vertices[vertex_indices[i]].depth;
        }
        this.depth /= 3;

        // Allocate and compute the level63 and floodlevel arrays;
        // they're just averages of the corresponding values at the
        // Triangle's vertices.
        this.level63 = new double[timesteps];
        this.floodlevel = new double[timesteps];
        Vertex p;
        for (int time=0; time<timesteps; ++time) {
            this.level63[time] = 0;
            this.floodlevel[time] = 0;
            for (int i=0; i<3; ++i) {
                p = vertices[vertex_indices[i]];
                this.level63[time] += p.level63[time];
                this.floodlevel[time] += p.floodlevel[time];
            }
            this.level63[time] /= 3;
            this.floodlevel[time] /= 3;

            //      if (this.level63[time] < -10000) {
            //System.out.printf("level63[%1d]=%f, floodlevel[%1d]=%f\n", time, this.level63[time], time, this.floodlevel[time]);
            //System.out.printf("  vertex level63s: %f, %f, %f\n",
            //            vertices[this.vertex_indices[0]].level63[time],
            //            vertices[this.vertex_indices[1]].level63[time],
            //            vertices[this.vertex_indices[2]].level63[time]);
            //System.out.printf("  vertex floodlevels: %f, %f, %f\n",
            //            vertices[this.vertex_indices[0]].floodlevel[time],
            //            vertices[this.vertex_indices[1]].floodlevel[time],
            //            vertices[this.vertex_indices[2]].floodlevel[time]);
            //      }

        }
    }

    /**
     * Test whether this triangle lies within a given lon/lat Box.
     *
     * @param vertices     Array of vertices for the mesh
     * @param box          Lat/lon box
     * @returns            True if all 3 vertices are inside the box, false otherwise.
     *                     (The edges of the box count as inside in this test.)
     */
    public boolean lies_within_box(Vertex vertices[], Box box) {
        Vertex p;
        for (int i=0; i<3; ++i) {
            p = vertices[vertex_indices[i]];
            if (p.x < box.xmin || p.y < box.ymin || p.x > box.xmax || p.y > box.ymax) { return false; }
        }
        return true;
    }
    

}
