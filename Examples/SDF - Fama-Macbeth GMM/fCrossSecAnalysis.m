function [firstStage,secondStage] = fCrossSecAnalysis(ret,riskFactors,flagAndrews,nLags)
% Purpose:  Estimate linear asset pricing models using the Fama and MacBeth
%           (1973) two-pass cross-sectional regression methodology and GMM
%           standard errors that corrects for errors-in-variables,
%           autocorrelation and heteroskedasticity.
% 
%
% Input:    ret         = TxN maxtrix of portfolio excess returns
%           riskFactors = TxK matrix of common risk factors
%           nLags       = Scalar indicating the number of lags to include in HAC
%           estimator of variance in first-stage regression and in
%           second-stage regression if flagAndrews = 0. 
%           flagAndrews = flag for computing optimal lag length using Andrews: 
%                        if flagAndrews = 1 it estimates the optimal lag
%                        length and flagAndrews = 0 do not, but uses
%                        instead nLags. 
%
%
% Output:   Two structures including results from the two steps
%               

%% Setting preliminaries

% Getting data dimensions
[nObs,nSeries]  = size(ret);
nFactors        = size(riskFactors,2);

%% First-pass time series regressions

% Estimating factor betas in multivariatye regression (with constant always)
tsReg               = nwRegress(ret,riskFactors,1,nLags);

% Constructing struct with results
firstStage.beta     = tsReg.bv;
firstStage.tstat    = tsReg.tbv;
firstStage.r2       = tsReg.R2v;
firstStage.r2adj    = tsReg.R2vadj;

%% Second-pass cross-sectional regressions

% Preallocations
tsGamma    = NaN(nObs,nFactors+1);

% Estimating lambda for each time period (assuming constant factor betas)
for iObs = 1:nObs

    tsGamma(iObs,:)  = [ones(nSeries,1) firstStage.beta(2:end,:)']\ret(iObs,:)';

end

% Estimating risk prices and Fama-MacBeth t-statistics
riskPrices      = mean(tsGamma);
covGamma        = ((tsGamma - repmat(riskPrices,nObs,1))'*(tsGamma - repmat(riskPrices,nObs,1)))/nObs^2;
seGamma         = sqrt(diag(covGamma))';
tGammaFM        = riskPrices./seGamma;

% Computing fitted values and R2 
meanReturns     = mean(ret);
fittedValues    = [ones(nSeries,1) firstStage.beta(2:end,:)']*riskPrices';
errResid        = mean(ret)' - fittedValues;
s2              = mean(errResid.^2);
vary            = mean((meanReturns - ones(1,nSeries) * mean(meanReturns)).^2);
rSquared        = 100*(1-s2./vary)';
MAPE            = mean(abs(errResid));
RMSE            = (mean(errResid.^2))^(1/2);


%% Compute GMM standard errors 

% Set up moment conditions: Time series regressions
epsilon     = ret - [ones(nObs,1) riskFactors]*firstStage.beta; 
mom1        = kron(epsilon,ones(1,nFactors+1)) .* kron(ones(1,nSeries),[ones(nObs,1) riskFactors]);

% Set up moment conditions: Cross-sectional regression
u           = bsxfun(@minus,ret,([ones(nSeries,1) firstStage.beta(2:end,:)']*riskPrices')');    
mom2        = u; 

% Weight matrix 
chi         = [ones(nSeries,1) firstStage.beta(2:end,:)'];
e           = [eye(nSeries*(nFactors+1)) zeros(nSeries*(nFactors+1),nSeries);...
              zeros(nFactors+1,nSeries*(nFactors+1)) chi'];

% Gather joint momements
moments     = [mom1 mom2];          

% Estimating the long-run covariance matrix
S            = fLongRunHac(moments,flagAndrews,nLags);

% Construct gradient matrix D
Dupper      = [1 mean(riskFactors);...
              mean(riskFactors)' nObs^(-1)*(riskFactors'*riskFactors)];
D11         = kron(eye(nSeries),Dupper);
D12         = zeros(nSeries*(nFactors+1),nFactors+1);
D21         = kron(eye(nSeries),[0 riskPrices(2:end)]);
D22         = chi;  
D           = -[ D11 D12 ; D21 D22 ];

% Estimating the covariance matrix for theta (all parameters)
thetaCov      = nObs^(-1)*((e*D)\(e*S*(e'))/(D'*(e'))); 

% Pick out covariance related to the risk prices (lower corner matrix)
idx         = nSeries*(nFactors+1)+1:nSeries*(nFactors+1)+nFactors+1;
gammaCov    = thetaCov(idx,idx);

% GMM standard errors and t-statistics
seGammaGMM  = sqrt(diag(gammaCov))';
tGammaGMM   = riskPrices./seGammaGMM;



%% Gathering outptut
secondStage.gamma           = riskPrices;
secondStage.seGammaFM       = seGamma; 
secondStage.tstatFM         = tGammaFM;
secondStage.seGammaGMM      = seGammaGMM; 
secondStage.tstatGMM        = tGammaGMM;
secondStage.r2              = rSquared;
secondStage.fit             = fittedValues;
secondStage.mean            = meanReturns';
secondStage.MAPE            = MAPE;
secondStage.RMSE            = RMSE;

end
