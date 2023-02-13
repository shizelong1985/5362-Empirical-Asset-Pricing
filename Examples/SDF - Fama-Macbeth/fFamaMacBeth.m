function [firstStage,secondStage] = fFamaMacBeth(ret,riskFactors,xscons,nLagsTS)

%% famaMacBeth.m
% ########################################################################### %
% function  [firstStage,secondStage] = famaMacBeth(ret,riskFactors,xscons)
% Purpose:  Estimate linear asset pricing models using the Fama and MacBeth
%           (1973) two-pass cross-sectional regression methodology. It also
%           provides adjusted standard errors and t-statistics based on
%           Shanken's (1992) errors-in-variables correction. 
%
% Input:    ret         = TxN maxtrix of portfolio excess returns
%           riskFactors = TxK matrix of common risk factors
%           xscons      = Flag for adding/omitting intercept in CS regression
%               - No cross-sectional intercept: xscons = "false"  
%               - Include cross-sectional intercept: xscons = "true" 
%           nLagTS = Scalar indicating the number of lags to include in HAC
%           estimator of variance in first-stage regression
%
%
% Output:   Two structures including results from the two steps


% Error checking on input parameters
if (nargin < 4)
    error('famaMacBeth.m: Not enough input parameters');
end

if (nargin > 4)
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

%% Second-pass cross-sectional regressions

if strcmp(xscons,'false')

    % Pre-allocations
    tsGamma    = NaN(nObs,nFactors);

    % Estimating lambda for each time period (assuming constant factor betas)
    for iObs = 1:nObs

        tsGamma(iObs,:)  = firstStage.beta(2:end,:)'\ret(iObs,:)';

    end

    % Estimating risk prices and Fama-MacBeth t-statistics
    riskPrices      = mean(tsGamma);
    covGamma        = ((tsGamma - repmat(riskPrices,nObs,1))'*(tsGamma - repmat(riskPrices,nObs,1)))/nObs^2;
%     seGamma         = sqrt(sum((tsGamma-repmat(riskPrices,nObs,1)).^2)/nObs^2);
    seGamma         = sqrt(diag(covGamma))';
    tGammaFM        = riskPrices./seGamma;
    
    % Adding a Shanken (1992) corrections as per Goyal (2012) eq. (33)
    covRiskFactors  = ((riskFactors - repmat(mean(riskFactors),nObs,1))'*(riskFactors - repmat(mean(riskFactors),nObs,1)))/(nObs-nFactors);
    c               = riskPrices/covRiskFactors*riskPrices'; 
    covShanken      = nObs^(-1)*((1+c)*(nObs*covGamma) + covRiskFactors);
    seGammaShanken  = sqrt(diag(covShanken))';
    tGammaShanken   = riskPrices./seGammaShanken; 

    % Computing fitted values and R2 
    meanReturns     = mean(ret);
    fittedValues    = firstStage.beta(2:end,:)' * riskPrices';
    errResid        = mean(ret)' - fittedValues;
    s2              = mean(errResid.^2);
    vary            = mean((meanReturns - ones(1,nSeries) * mean(meanReturns)).^2);
    rSquared        = 100*(1-s2./vary)';
    MAPE            = mean(abs(errResid));
    RMSE            = (mean(errResid.^2))^(1/2);

elseif strcmp(xscons,'true')

    % Preallocations
    tsGamma    = NaN(nObs,nFactors+1);

    % Estimating lambda for each time period (assuming constant factor betas)
    for iObs = 1:nObs

        tsGamma(iObs,:)  = [ones(nSeries,1) firstStage.beta(2:end,:)']\ret(iObs,:)';

    end

    % Estimating risk prices and Fama-MacBeth t-statistics
    riskPrices      = mean(tsGamma);
    covGamma        = ((tsGamma - repmat(riskPrices,nObs,1))'*(tsGamma - repmat(riskPrices,nObs,1)))/nObs^2;
%     seGamma         = sqrt(sum((tsGamma-repmat(riskPrices,nObs,1)).^2)/nObs^2);
    seGamma         = sqrt(diag(covGamma))';
    tGammaFM        = riskPrices./seGamma;
    
    % Adding a Shanken (1992) corrections as per Goyal (2012) eq. (33)
    covRiskFactors      = ((riskFactors - repmat(mean(riskFactors),nObs,1))'*(riskFactors - repmat(mean(riskFactors),nObs,1)))/(nObs-nFactors);
    covRiskFactorstilde = [zeros(nFactors,1)';covRiskFactors];
    covRiskFactorstilde = [zeros(nFactors+1,1),covRiskFactorstilde];
    c               = riskPrices*pinv(covRiskFactorstilde)*riskPrices'; 
    covShanken      = nObs^(-1)*((1+c)*(nObs*covGamma) + covRiskFactorstilde);
    seGammaShanken  = sqrt(diag(covShanken))';   
    tGammaShanken   = riskPrices./seGammaShanken; 
    
    % Computing fitted values and R2 
    meanReturns     = mean(ret);
    fittedValues    = [ones(nSeries,1) firstStage.beta(2:end,:)']*riskPrices';
    errResid        = mean(ret)' - fittedValues;
    s2              = mean(errResid.^2);
    vary            = mean((meanReturns - ones(1,nSeries) * mean(meanReturns)).^2);
    rSquared        = 100*(1-s2./vary)';
    MAPE            = mean(abs(errResid));
    RMSE            = (mean(errResid.^2))^(1/2);

end

% Constructing structure with results
secondStage.gamma           = riskPrices;
secondStage.seGammaFM       = seGamma; 
secondStage.seGammaShanken  = seGammaShanken;
secondStage.tstatFM         = tGammaFM;
secondStage.tstatShanken    = tGammaShanken;
secondStage.r2              = rSquared;
secondStage.fit             = fittedValues;
secondStage.mean            = meanReturns';
secondStage.MAPE            = MAPE;
secondStage.RMSE            = RMSE;
secondStage.cShanken        = c;

end

% ########################################################################### %
% [EOF]
% ########################################################################### %