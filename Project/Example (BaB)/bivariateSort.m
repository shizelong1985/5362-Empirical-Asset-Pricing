function out = bivariateSort(returns,signals1,signals2,prctVal1,prctVal2,sType,wType,meq,code,exchcds)

%% bivariateSort.m
% ########################################################################### %
% function  out = bivariateSort(returns,signals1,signals2,prctVal1,prctVal2,...
%                 sType,wType,meq,code,exchcds)
%
% Purpose:  Sort asset into portfolios based on two observable signals
%
% Input:    returns         = T x N matrix of portfolio (excess) returns
%           signals1        = T x N matrix of first sorting signals
%           signals2        = T x N matrix of second sorting signals
%           prctVal1        = Percentiles for the first dimension
%           prctVal2        = Percentiles for the second dimension
%           sType           = String indicating (un)conditional sort
%           wType           = String indicating equal- or value-weighted
%           meq             = T x N matrix of market equity for value-weighting 
%           code            = String indicating NYSE or ALL stock breakpoints
%           exchcds         = T x N matrix of exchange codes
%
% Output:   out             = Struct with portfolio returns, placements, and 
%                             breakpoints for portfolio formation
%               
% Written by:
% Jonas N. Eriksen
% Department of Economics and Business Economics
% Aarhus University and CREATES
%
% Encoding: UTF8
% Last modified: February, 2021
%
% ONLY INTENDED FOR USE IN THE MASTER'S COURSE "EMPIRICAL ASSET PRICING". 
% ########################################################################### %

%% Error checking on user input
if (nargin > 10)
    error('bivariateSort.m: Too many input arguments');
end

if (nargin < 7)
    error('bivariateSort.m: Not enough input arguments');
end

if (~ismember(wType,[{'EW'},{'VW'}]))
    error('bivariateSort.m: Unsupported weighting type');
end

if (nargin < 8) && strcmp(wType,'VW')
    error('bivariateSort.m: Market capitalization not supplied');
end

if (~ismember(sType,[{'Unconditional'},{'Conditional'}]))
    error('bivariateSort.m: Unsupported sorting type');
end

if (nargin > 8) && (~isempty(code)) && (~ismember(code,[{'NYSE'},{'ALL'}]))
    error('bivariateSort.m: Unsupported exchange code definition');
end

if (nargin == 9) && (ismember(code,{'NYSE'}))
    error('bivariateSort.m: Exchange codes not supplied');
end

if (size(returns,1) ~= size(signals1,1)) || (size(returns,2) ~= size(signals1,2))
    error('bivariateSort.m: Returns and signals1 not conforming');   
end

if (size(returns,1) ~= size(signals1,1)) || (size(returns,2) ~= size(signals1,2))
    error('bivariateSort.m: Returns and signals2 not conforming');   
end

if (nargin == 10) && (size(returns,1) ~= size(exchcds,1)) || (nargin == 10) && (size(returns,2) ~= size(exchcds,2))
    error('bivariateSort.m: Returns and exchange codes not conforming');   
end

if (nargin > 7 && (size(returns,1) ~= size(meq,1))) || (nargin > 7 && (size(returns,2) ~= size(meq,2)))
    error('bivariateSort.m: Returns and market values not conforming');   
end

if (isequal(signals1(~isnan(signals1)),signals2(~isnan(signals2))))
    error('bivariateSort.m: Provided signals are identical');
end

if (~ismember(mean(prctVal1),0:100)) || (~ismember(mean(prctVal1),0:100))
    error('bivariateSort.m: Percentiles must be between 0 and 100');
end

% Setting preliminaries
if (nargin < 9)
    code = 'ALL';
end

%% Building portfolios
% ########################################################################### %
%{
    We first obtain data dimensions and preallocate prior to looping 
    procedure. Next, we run over different time periods and place assets
    in portfolios according to the ranking of their signals. Portfolios can
    either be equal-weighted or value-weighted and the sorting procedure can
    either be unconditional or conditional. In the unconditinal sort, the 
    ordering of signal1 and signal2 does not matter, but they do in the 
    conditional sorting procedure. 
%}
% ########################################################################### %

% Getting number of portfolios from percentile values
numPf1          = size(prctVal1,2) + 1;
numPf2          = size(prctVal2,2) + 1;

% Getting data dimensions and setting preallocations
[nObs,nAss]     = size(returns);
idxMat          = NaN(nObs,nAss);
bpMat1          = NaN(nObs,numPf1-1);
bpMat2          = NaN(nObs,numPf2-1);
retPortfolio    = NaN(nObs,numPf1*numPf2);

% Building portfolios returns
for iObs = 2:nObs
    
    % Check if there are any non-NaN signals
    if sum(isnan(signals1(iObs-1,:))) == nAss || sum(isnan(signals2(iObs-1,:))) == nAss

        % Set portfolio returns to NaN if all signals are NaN
        retPortfolio(iObs,:) = NaN;

    % Check if there are enough assets to create nPortfolios
    elseif sum(~isnan(returns(iObs,:))) < numPf1*numPf2

        % Set portfolio returns to NaN is number of assets 
        % is lower than the number of portfolios
        retPortfolio(iObs,:) = NaN;

    else

        % Set signals to NaN if there is no return in subsequent month
        signals1(iObs-1,isnan(returns(iObs,:))) = NaN;
        signals2(iObs-1,isnan(returns(iObs,:))) = NaN;
        
        % Set signals to NaN if there is no market equity in subsequent month
        if (nargin == 10)
            signals1(iObs-1,isnan(meq(iObs-1,:))) = NaN;
            signals2(iObs-1,isnan(meq(iObs-1,:))) = NaN;
        end

        % Determining portfolio allocations using an unconditional sort
        if strcmp(sType,'Unconditional')

            % Setting preliminaries
            idx             = NaN(1,nAss);
            count           = 0;

            % Determining breakpoints and bins along first dimension
            if strcmp(code,'NYSE')
                breakpoints1    = prctile(signals1(iObs-1,ismember(exchcds(iObs-1,:),1)),prctVal1);
            elseif strcmp(code,'ALL')
                breakpoints1    = prctile(signals1(iObs-1,:),prctVal1);
            end

            % Saving breakpoint to matrix for output
            bpMat1(iObs,:)  = breakpoints1;

            % Determine portfolio placement of the individual securities
            bins1           = discretize(signals1(iObs-1,:),[-inf breakpoints1 inf]);

            % Determining breakpoints and bins along second dimension
            if strcmp(code,'NYSE')
                breakpoints2    = prctile(signals2(iObs-1,ismember(exchcds(iObs-1,:),1)),prctVal2);
            elseif strcmp(code,'ALL')
                breakpoints2    = prctile(signals2(iObs-1,:),prctVal2);
            end

            % Saving breakpoint to matrix for output
            bpMat2(iObs,:)  = breakpoints2;

            % Determine portfolio placement of the individual securities
            bins2           = discretize(signals2(iObs-1,:),[-inf breakpoints2 inf]);

            % Determining portfolio placements            
            for ii = 1:numPf1

                for jj = 1:numPf2

                    % Determing the portfolio placement
                    count                               = count + 1;
                    tmp                                 = count.*(bins1 == ii & bins2 == jj);
                    idx(1,bins1 == ii & bins2 == jj)    = tmp(1,bins1 == ii & bins2 == jj);

                end
                
            end
        
        % Determining portfolio allocations using a conditional sort
        elseif strcmp(sType,'Conditional')

            % Setting preliminaries
            idx             = NaN(1,nAss);

            % Determining breakpoints and bins along first dimension
            if strcmp(code,'NYSE')
                breakpoints1    = prctile(signals1(iObs-1,ismember(exchcds(iObs-1,:),1)),prctVal1);
            elseif strcmp(code,'ALL')
                breakpoints1    = prctile(signals1(iObs-1,:),prctVal1);
            end

            % Saving breakpoint to matrix for output
            bpMat1(iObs,:)  = breakpoints1;

            % Determine portfolio placement of the individual securities
            bins1        = discretize(signals1(iObs-1,:),[-inf breakpoints1 inf]);

            % Determining portfolio placements            
            for ii = 1:numPf1

                % Determining breakpoints and bins along second dimension conditional on the first dimension
                if strcmp(code,'NYSE')
                    breakpoints2   = prctile(signals2(iObs-1,bins1 == ii & ismember(exchcds(iObs-1,:),1)),prctVal2);
                elseif strcmp(code,'ALL')
                    breakpoints2   = prctile(signals2(iObs-1,bins1 == ii),prctVal2);
                end

                % Saving breakpoint to matrix for output
                bpMat2(iObs,:)  = breakpoints2;

                % Determing the portfolio placement
                idx(1,bins1 == ii) = ((ii-1).*numPf2) + discretize(signals2(iObs-1,bins1 == ii),[-inf breakpoints2 inf]);

            end

        end

        % Saving portfolio placement to matrix
        idxMat(iObs,:)  = idx;

        % Computing portfolio returns according to chosen weights
        if strcmp(wType,'EW')
    
            % Computing equal-weighted portfolio returns
            retPortfolio(iObs,:) = accumarray(idx(~isnan(idx))',returns(iObs,~isnan(idx)),[],@nanmean);

        elseif strcmp(wType,'VW')

            % Computing sum of market values
            meqSum = accumarray(idx(~isnan(idx))',meq(iObs-1,~isnan(idx)));

            % Computing value-weighted portfolio returns
            retPortfolio(iObs,:) = accumarray(idx(~isnan(idx))',...
                returns(iObs,~isnan(idx)).*meq(iObs-1,~isnan(idx))./meqSum(idx(~isnan(idx))).');

        end

    end

end

% Create struct for the output
out.returns         = retPortfolio;
out.placement       = idxMat;
out.breakpoints.s1  = bpMat1;
out.breakpoints.s2  = bpMat2;

end

% ########################################################################### %
% [EOF]
% ########################################################################### %