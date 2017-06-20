function [zGrid, zError] = optimalInterp(xObs, yObs, zObs, xGrid, yGrid, ...
                                         Lx, Ly, obsNoise)
% OPTIMALINTERP - Interpolates a scalar field between two sets of point locations.
%  This function uses a statiscally optimal approach to 
%  interpolate a 2D scalar field between two sets of locations, given the
%  spatial correlation scales in each direction.
% 
%  It also returns information of the statistical error on such interpolation.
% 
%  Syntax: [zGrid, zError] = optimalInterp(xObs, yObs, zObs, xGrid, yGrid, ...
%                                Lx, Ly, obsNoise)
%
%  Inputs:
%     [xObs yObs zObs] - zObs is the value observed at point [xObs yObs]
%     [xGrid yGrid] - points were interpolated values are desired
%     [Lx, Ly] - radius of influence in two different axis (Gaussian function)
%     obsNoise - signal to noise ratio
% 
%  Outputs:
%     [zGrid zError] = interpolated value and standard error
% 
%  Example:
%
%     % Generate an arbitrary 2D scalar field
%     [x, y] = meshgrid(linspace(0, 1, 100));
%     z = (x.^2 + 2* y.^2 + 3) .* cos(x.*y);
% 
%     % Get observations at randomm locations 
%     xObs = rand(200, 1);
%     yObs = rand(200, 1);
%     zObs = interp2(x, y, z, xObs, yObs, 'linear');
% 
%     % Try to estimate the 2D field from the observations
%     [zInterp, zError] = optimalInterp(xObs, yObs, zObs, x(:), y(:), 0.05, 0.05, 0.05);
%     zInterp = reshape(zInterp, size(x));
%     zError  = reshape(zError, size(x));
% 
%     figure(1); clf;
%     subplot(2, 2, 1); hold on;
%     pcolor(x, y, z); shading flat; colorbar;
%     plot(xObs, yObs, 'ko');
%     axis equal tight;
% 
%     subplot(2, 2, 2);
%     scatter(xObs, yObs, 10, zObs, 'filled'); colorbar;
%     axis equal tight;
% 
%     subplot(2, 2, 3);
%     pcolor(x, y, zInterp); shading flat; colorbar;
%     axis equal tight;
% 
%     subplot(2, 2, 4);
%     pcolor(x, y, zError); shading flat; colorbar;
%     axis equal tight;
%
%
% 
%  Other m-files required: none
%  Subfunctions: GETAVAILABLESPACE, BUILDPOLINOMIALTERMS, 
%                DETRENDVAR, RETRENDVAR, COMPUTEANALYSIS,
%                TESTNORMALITY, NORMALCDF, KOLMOGOROVSMIRNOVCDF
%  MAT-files required: none
% 
%  See also: PREPAREGRIDDATA, PERFORMGRIDDATA, GRIDDATA, INTERP2
% 
%  Author: Bartolome Garau
%  Author e-mail: tgarau@oceandrivers.com
%  Website: http://www.oceandrivers.com
%  Creation: 15-Sep-2012
% 

%% Remove input observations trend
    % One of the requirements of the objective analysis technique
    % is that observations must follow a Normal distribution
    isNormal =  0;
    degree   = -1;
    while and(~isNormal, degree < 2)
        degree = degree + 1;
        [zObsNew, trendCoefs] = detrendVar(xObs, yObs, zObs, degree);
        % If Statistics toolbox is present, use this text (more appropiate)
        if exist('lillietest', 'file')
            h = lillietest(zObsNew);
            isNormal = (h == 0);
        else % If not, use this homemade one (implemented below)
            pVal = testNormality(zObsNew);
            isNormal = (pVal > 0.5);
        end;
    end;

    if ~isNormal
        disp('Warning! Detrending the input did not succeed at converting it to a Gaussian distribution');
    end;
    
%% Interpolate the residuals using weights computed from covariance matrices
    [zGrid, zError] = innerOptimalInterp(xObs, yObs, zObsNew, xGrid, yGrid, ...
                                         Lx, Ly, obsNoise); 

%% Put trend back into the interpolated field
    zGrid = retrendVar(xGrid, yGrid, zGrid, trendCoefs, degree);
    return;

%% SUBFUNCTIONS

    function [zGrid, zError] = innerOptimalInterp(xObs, yObs, zObs, xGrid, yGrid, ...
                                         Lx, Ly, obsNoise)
    
        % Parameters of the data to be interpolated
        numObsPts = length(xObs);
        numGrdPts = length(xGrid);

        maxElems = getAvailableSpace;
        if (numObsPts^2 > maxElems), % Suboptimal: divide in observations groups, then fuse

            %disp('Too much observations -> Suboptimal (splitting observations in subsets)');

            curRange = 1:2:numObsPts;
            [zGrid1, zError1] = innerOptimalInterp(xObs(curRange), yObs(curRange), zObs(curRange), ...
                xGrid, yGrid, Lx, Ly, obsNoise);

            curRange = 2:2:numObsPts;
            [zGrid2, zError2] = innerOptimalInterp(xObs(curRange), yObs(curRange), zObs(curRange), ...
                xGrid, yGrid, Lx, Ly, obsNoise);

            zGrid = (zGrid1 .* zError2 + zGrid2 .* zError1) ./ (zError1 + zError2);
            zError = (zError1 + zError2) / 2;

        else % Optimal: use whole set of observations

            % Precomputation of some constants
            gaussDenomX = 2 * Lx^2;
            gaussDenomY = 2 * Ly^2;

            % Computing covariance matrix between observation points
            gamma = obsNoise ./ var(zObs);
            dx = xObs * ones(1, numObsPts) - ones(numObsPts, 1) * xObs';
            dy = yObs * ones(1, numObsPts) - ones(numObsPts, 1) * yObs';
            expo = (dx.^2)./ gaussDenomX + (dy.^2)./ gaussDenomY;
            CovObservations = exp(-expo) + gamma * eye(numObsPts);
            clear('dx','dy','expo');

            zGrid = zeros(size(xGrid));
            zError = zeros(size(xGrid));

            maxElems = getAvailableSpace;
            if (numGrdPts * numObsPts > maxElems), % Divide analysis points in groups
                % Decide the size of each group
                lag = floor(sqrt(maxElems)/2);
                for rangeStart = 1:lag:numGrdPts

                    partialRange = rangeStart:min(rangeStart + lag - 1, numGrdPts);

                    [zPartGrid, zPartError] = computeAnalysis(xObs, yObs, zObs, ...
                        xGrid(partialRange), yGrid(partialRange), ...
                        gaussDenomX, gaussDenomY, CovObservations);

                    % Save this partial result
                    zGrid(partialRange) = zPartGrid;
                    zError(partialRange) = zPartError;
                    clear('zPartGrid','zPartError');
                end;
            else % Not necessary to divide analysis points in groups
                [zGrid, zError] = computeAnalysis(xObs, yObs, zObs, ...
                    xGrid, yGrid, gaussDenomX, gaussDenomY, CovObservations);
            end;
        end;
                                     
    end

    function maxElems = getAvailableSpace
        % This should be wiser, but due to differences between Matlab
        % releases, OS, etc. just made a stupid approach. Feel free 
        % to change this in order to adapt it to your system
        
        %pack;
        maxblock = 1000^2; %maxblock = feature('dumpmem');
        elemSize = 1;
        maxElems = (maxblock / elemSize);
        return;
    end

    function lhSide = buildPolinomialTerms(x, y, degree)
        lhSide = [];
        for currentDegree = degree:-1:0
            for leftFactorDegree = currentDegree:-1:0
                leftFactor  = x .^ leftFactorDegree;
                rightFactor = y .^ (currentDegree - leftFactorDegree);
                currentTerm = leftFactor .* rightFactor;
                lhSide = [lhSide, currentTerm]; %#ok<AGROW>
            end;
        end;
        return;
    end

    function [detrendedVar, trendCoefs] = detrendVar(xObs, yObs, zObs, degree)

        % Generate all the polinomial terms
        lhSide = buildPolinomialTerms(xObs, yObs, degree);
        % Find coefficients for the least-squares fit
        trendCoefs = lhSide \ zObs; 
        % Compute the approximation obtained from the fit
        approxVar = lhSide * trendCoefs; 
        % Remove the trend from the observed values
        detrendedVar = zObs - approxVar;
        return;
    end

    function varValues = retrendVar(xGrid, yGrid, zGrid, trendCoefs, degree)
        % Generate all the polinomial terms
        lhSide = buildPolinomialTerms(xGrid, yGrid, degree);
        % Compute the approximation obtained from the fit
        trend = lhSide * trendCoefs; 
        % Remove the trend from the observed values
        varValues = zGrid + trend;
        return;
    end

    function [zGrid, zError] = computeAnalysis(xObs, yObs, zObs, ...
            xGrid, yGrid,...
            gaussDenomX, gaussDenomY, CovObservations)

        numObs = length(xObs);
        numGrd = length(xGrid);

        % Computing covariance matrix between observation and grid points
        deltax   = xObs * ones(1, numGrd) - ones(numObs, 1) * xGrid';
        deltay   = yObs * ones(1, numGrd) - ones(numObs, 1) * yGrid';
        exponent = (deltax.^2)./ gaussDenomX + (deltay.^2)./ gaussDenomY;
        CovAnalysis = exp(-exponent);
        clear('deltax','deltay','exponent');

        % Matrix inversion to find the optimum weights
        leftHandSide = [CovObservations, ones(numObs, 1);
                        ones(1, numObs),               0];
        rightHandSide = [CovAnalysis;    ones(1, numGrd)];
        Weights = leftHandSide \ rightHandSide; % weights (+ lagrangian multiplier)
        clear('leftHandSide');

        % Interpolate the field and compute the error
        zGrid  = Weights(1:numObs, :)' * zObs;    % interpolated value
        zError = 1 - diag(Weights' * rightHandSide); % percent error 
        clear('rightHandSide');
        zError = std(zObs) * (zError.^0.5);    % scaled to the observation variance 
        return;
    end

    function pval = testNormality(origDataset)
    % Perform a Kolmogorov-Smirnov test of the null hypothesis that the
    % dataset x comes from the (continuous) normal distribution. I.e.,
    % if F and G are the CDFs corresponding to the sample and gaussian,
    % respectively, then the null is that F == G.
    %
    % The p-value of the test is returned.


        sortedDs = sort(origDataset(:));
        sortedDs = sortedDs(~isnan(sortedDs));

        mu       = mean(sortedDs);
        sigma    = std(sortedDs);
        sortedDs = (sortedDs - mu) ./ sigma;
        n        = length(sortedDs);
        z        = normalCdf(sortedDs);
        dist     = [abs(z - (0:(n - 1))' / n); 
                    abs(z - (1:n)'       / n)];
        ks       = sqrt(n) * max(dist(:));
        pval     = 1 - kolmogorovSmirnovCdf(ks);
        return;
    end

    function cdf = normalCdf(dataset)
        % For each element of dataset, compute the cumulative distribution
        % function (CDF) at dataset of the normal distribution N(0, 1)
        cdf = (1 + erf (dataset / sqrt(2))) / 2;
        return;
    end

    function cdf = kolmogorovSmirnovCdf(dataset)
        % For each element of dataset, compute the cumulative distribution
        % function (CDF) at dataset of the Kolmogorov-Smirnov distribution
        %            Inf
        % cdf(x) =   SUM      (-1)^k exp(-2 k^2 x^2)
        %            k = -Inf
        % for positive values of the dataset

        cdf = zeros(size(dataset));
        idx = find (dataset > 0);
        if ~isempty(idx)
            y = dataset(idx);
            y = y(:);
            K   = ceil(sqrt(-log(eps) / 2) / min(y));
            k   = (1:K)';
            A   = exp (-2 * k.^2 * y.^2);
            odd = find (rem (k, 2) == 1);
            A(odd, :) = -A(odd, :);
            cdf(idx)  = 1 + 2 * sum (A);
        end

        return;
    end

end
