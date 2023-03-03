function [firstStage,secondStage] = fFamaMacBeth_standard(ret,riskFactors,xscons,nLagsTS,varargin)

%% famaMacBeth.m
% ########################################################################### %
% function  [firstStage,secondStage] = famaMacBeth(ret,riskFactors,xscons)
% Purpose:  Estimate linear asset pricing models using the Fama and MacBeth
%           (1973) two-pass cross-sectional regression methodology.
%
% Input:    ret         = TxN maxtrix of portfolio excess returns
%           riskFactors = TxK matrix of common risk factors
%           xscons      = Flag for adding/omitting intercept in CS regression
%               - No cross-sectional intercept: xscons = "false"  
%               - Include cross-sectional intercept: xscons = "true" 
%           nLagTS = Scalar indicating the number of lags to include in HAC
%           estimator of variance in first-stage regression
%           varargin     = optional argument with firm characteristics that
%           will be included only in the second stage, using their means.
%           The input should be a cell with each element a TxN matrix of
%           characteristics.
%
%
% Output:   Two structures including results from the two steps
%

% Error checking on input parameters
if (nargin < 4)
    error('famaMacBeth.m: Not enough input parameters');
end

if (nargin > 5)
   error('famaMacBeth.m: Too many input parameters');
end

if (size(ret,1) ~= size(riskFactors,1))
    error('famaMacBeth.m: Unequal number of time series observations');
end

if (nargin == 3) && ~ismember(xscons,{'false','true'})
    error('famaMacBeth.m: xscons should either be "false" or "true"');
end


%% Setting preliminaries
% ########################################################################### %
%{
    Obtain data dimensions for returns and risk factors, and construct
    vector of ones for use in first-step regression
%}
% ########################################################################### %

% Getting data dimensions
[nObs,nSeries]  = size(ret);
nFactors        = size(riskFactors,2);

%% First-pass time series regressions

% Estimating factor betas in multivariatye regression (with constant always)
tsReg               = nwRegress(ret,riskFactors,1,nLagsTS);

% Constructing struct with results
firstStage.beta     = tsReg.bv;
firstStage.tstat    = tsReg.tbv;
firstStage.r2       = tsReg.R2v;
firstStage.r2adj    = tsReg.R2vadj;

%% Identify characteristics 
nChar      = 0;
if (nargin == 5)
    
    % Ensure same dimension of cell input
    if size(varargin{:},1) > size(varargin{:},2)
        varargin{:} = varargin{:}';
    end
    
    [~,nChar]    = size(varargin{:});
    
    char         = NaN(nChar,nSeries);
    temp         = varargin{1,1};
    for p=1:nChar
        char(p,:)  = mean(temp{1,p});
    end
    
end

%% Second-pass cross-sectional regressions

if strcmp(xscons,'false')

    % Pre-allocations
    tsGamma    = NaN(nObs,nFactors+nChar);

    % Estimating gamma for each time period (assuming constant factor betas)
    for iObs = 1:nObs
        
        if (nargin == 5) % if characteristics are included
            tsGamma(iObs,:)  = [firstStage.beta(2:end,:)' char']\ret(iObs,:)';
        else
        	tsGamma(iObs,:)  = firstStage.beta(2:end,:)'\ret(iObs,:)';
        end

    end

    % Estimating risk prices and Fama-MacBeth t-statistics
    riskPrices      = mean(tsGamma);
    covGamma        = ((tsGamma - repmat(riskPrices,nObs,1))'*(tsGamma - repmat(riskPrices,nObs,1)))/nObs^2;
%     seGamma         = sqrt(sum((tsGamma-repmat(riskPrices,nObs,1)).^2)/nObs^2);
    seGamma         = sqrt(diag(covGamma))';
    tGammaFM        = riskPrices./seGamma;

    % Computing fitted values and R2 
    meanReturns     = mean(ret);
    if (nargin == 5)
        fittedValues    = [firstStage.beta(2:end,:)' char']*riskPrices';
    else
        fittedValues    = firstStage.beta(2:end,:)'*riskPrices';
    end
    errResid        = mean(ret)' - fittedValues;
    s2              = mean(errResid.^2);
    vary            = mean((meanReturns - ones(1,nSeries) * mean(meanReturns)).^2);
    rSquared        = 100*(1-s2./vary)';
    MAPE            = mean(abs(errResid));
    RMSE            = (mean(errResid.^2))^(1/2);

elseif strcmp(xscons,'true')

    % Preallocations
    tsGamma    = NaN(nObs,nFactors+1+nChar);

    % Estimating gamma for each time period (assuming constant factor betas)
    for iObs = 1:nObs
        if (nargin == 5) % if characteristics are included
            tsGamma(iObs,:)  = [ones(nSeries,1) firstStage.beta(2:end,:)' char']\ret(iObs,:)';
        else
            tsGamma(iObs,:)  = [ones(nSeries,1) firstStage.beta(2:end,:)']\ret(iObs,:)';
        end
    end
    
    % Estimating risk prices and Fama-MacBeth t-statistics
    riskPrices      = mean(tsGamma);
    covGamma        = ((tsGamma - repmat(riskPrices,nObs,1))'*(tsGamma - repmat(riskPrices,nObs,1)))/nObs^2;
%     seGamma         = sqrt(sum((tsGamma-repmat(riskPrices,nObs,1)).^2)/nObs^2);
    seGamma         = sqrt(diag(covGamma))';
    tGammaFM        = riskPrices./seGamma;

    % Computing fitted values and R2 
    meanReturns     = mean(ret);
    if (nargin == 5)
        fittedValues    = [ones(nSeries,1) firstStage.beta(2:end,:)' char']*riskPrices';
    else
        fittedValues    = [ones(nSeries,1) firstStage.beta(2:end,:)']*riskPrices';
    end
    errResid        = mean(ret)' - fittedValues;
    s2              = mean(errResid.^2);
    vary            = mean((meanReturns - ones(1,nSeries) * mean(meanReturns)).^2);
    rSquared        = 100*(1-s2./vary)';
    MAPE            = mean(abs(errResid));
    RMSE            = (mean(errResid.^2))^(1/2);

end

% Constructing structure with results
secondStage.gamma   = riskPrices;
secondStage.tstatFM = tGammaFM;
secondStage.r2      = rSquared;
secondStage.fit     = fittedValues;
secondStage.mean    = meanReturns';
secondStage.MAPE    = MAPE;
secondStage.RMSE    = RMSE;

end

% ########################################################################### %
% [EOF]
% ########################################################################### %