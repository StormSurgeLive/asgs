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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Reader63 {
    BufferedReader br;
    String bufferedLine;
    Pattern pat;
    public Reader63(String file) throws Exception {
        br = new BufferedReader(new FileReader(new File(file)));
        bufferedLine = null;
        // 43200.000000 432001 0.003712
        pat = Pattern.compile("^(\\S+\\s+\\S+)(1\\s+\\S+)$");
    }
    public String readLine() throws Exception {
        if (bufferedLine != null) {
            String s = bufferedLine;
            bufferedLine = null;
            return s;
        }
        String line;
        while (true) {
            line = br.readLine().trim();
            if (line == null) { break; }
            if (line.length() == 0) { continue; }
            Matcher m = pat.matcher(line);
            if (m.matches()) {
                line = m.group(1);
                bufferedLine = m.group(2);
            }
            break;
        }
        return line;
    }
    public void close() throws Exception {
        br.close();
    }
}
