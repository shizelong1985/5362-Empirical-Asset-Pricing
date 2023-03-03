function out = univariateSort(returns,signals,prctVal,wType,meq,code,exchcds)

%% univariateSort.m
% ########################################################################### %
% function  out = univariateSort(returns,signals,prctVal,wType,meq,code,exchcds)
%
% Purpose:  Sort individual asset into portfolios based on observable signals
%
% Input:    returns         = T x N matrix of portfolio excess returns
%           signals         = T x N matrix of sorting signals
%           prctVal         = Matrix of percentiles for portfolios
%           wType           = String indicating equal- or value-weighted
%           meq             = T x N matrix of market equity for value-weighting 
%           code            = String indicating NYSE or ALL stocks
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

%% Error checking
if (nargin > 7)
    error('univariateSort.m: Too many input arguments');
end

if (nargin < 4)
    error('univariateSort.m: Not enough input arguments');
end

if ~ismember(wType,[{'EW'},{'VW'}])
    error('univariateSort.m: Wrong portfolio type');
end

if (nargin < 5) && strcmp(wType,'VW')
    error('univariateSort.m: Market capitalization not supplied');
end

if (nargin > 5) && (~isempty(code)) && (~ismember(code,[{'NYSE'},{'ALL'}]))
    error('univariateSort.m: Unsupported exchange code definition');
end

if (nargin == 6) && (ismember(code,{'NYSE'}))
    error('univariateSort.m: Exchange codes not supplied');
end

if (size(returns,1) ~= size(signals,1)) || (size(returns,2) ~= size(signals,2))
    error('univariateSort.m: Return and signal dimensions are not consistent');   
end

if (nargin == 7) && (size(returns,1) ~= size(exchcds,1)) || (nargin == 7) && (size(returns,2) ~= size(exchcds,2))
    error('univariateSort.m: Returns and exchange codes not conforming');   
end

if (strcmp(wType,'VW') && (size(returns,1) ~= size(meq,1))) || (strcmp(wType,'VW') && (size(returns,2) ~= size(meq,2)))
    error('univariateSort.m: Returns and market values not conforming');   
end

if (~ismember(mean(prctVal),0:100))
    error('univariateSort.m: Percentiles must be between 0 and 100');
end

% Setting preliminaries
if (nargin < 6)
    code = 'ALL';
end

%% Building portfolios
% ########################################################################### %
%{
    We first obtain data dimensions and preallocate prior to the looping 
    procedure. Next, we run over different time periods and place assets
    in portfolios according to the ranking of their signal. Portfolios can
    either be equal-weighted or value-weighted. 
%}
% ########################################################################### %

nPortfolios     = size(prctVal,2) + 1;

% Getting data dimensions and setting preallocations
[nObs,nAss]     = size(returns);
idxMat          = NaN(nObs,nAss);
bpMat           = NaN(nObs,nPortfolios-1);
retPortfolio    = NaN(nObs,nPortfolios);

% Building portfolio returns
for iObs = 2:nObs

    % Check if there are any non-NaN signals
    if sum(isnan(signals(iObs-1,:))) == nAss

        % Set portfolio returns to NaN if all signals are NaN
        retPortfolio(iObs,:)    = NaN;

    % Check if there are enough assets to create nPortfolios
    elseif sum(~isnan(returns(iObs,:))) < nPortfolios

        % Set portfolio returns to NaN is number of assets 
        % is lower than the number of portfolios
        retPortfolio(iObs,:)    = NaN;

    else
        
        % Set signals to NaN if there is no return in subsequent month
        signals(iObs-1,isnan(returns(iObs,:))) = NaN;
        
        % Set signals to NaN if there is no market equity in subsequent month
        if (nargin > 4) && strcmp(wType,'VW')
            signals(iObs-1,isnan(meq(iObs-1,:))) = NaN;
        end

        % Determining breakpoints
        if strcmp(code,'NYSE')
            breakpoints     = prctile(signals(iObs-1,ismember(exchcds(iObs-1,:),1)),prctVal);
        elseif strcmp(code,'ALL')
            breakpoints     = prctile(signals(iObs-1,:),prctVal);
        end

        % Saving breakpoint to matrix for output
        bpMat(iObs,:) = breakpoints;

        % Determine portfolio placement of the individual securities
        idx     = discretize(signals(iObs-1,:),[-inf breakpoints inf]);
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
out.returns     = retPortfolio;
out.placement   = idxMat;
out.breakpoints = bpMat;

end

% ########################################################################### %
% [EOF]
% ########################################################################### %