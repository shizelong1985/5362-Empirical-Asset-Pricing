function regResults = nwRegress(y,x,constant,nlag)

%% nwRegress.m
% ########################################################################### %
% function regResults = nwRegress(y,x,constant,method,nlag)
% Purpose:  Estimate a linear regression model with Newey-West standard errors
%           If y is a matrix, then the function runs N seperate regressions
%
% Input:    y           = TxN matrix of dependent variables
%           x           = A TxK matrix of common explanatory variables
%           constant    = 1 to add constant internally, 0 otherwise
%           nlag        = Scalar indicating the number of lags in NW estimator
%
% Output:   A structure including
%           bv          = A KxN matrix of parameter estimates
%           sbv         = A KxN matrix of Newey-West standard errors
%           tbv         = A KxN matrix of Newey-West t-statistics
%           R2v         = A Nx1 vector of R-square values
%           R2vadj      = A Nx1 vector of adjusted R-square values
%           resid       = A TxN matrix of residuals
%
% Author:
% Jonas N. Eriksen
% Department of Economics and Business Economics
% Aarhus University and CREATES
%
% Encoding: UTF8
% Last modified: January, 2021
%
% NOTE: ONLY INTENDED FOR USE IN THE COURSE "EMPIRICAL ASSET PRICING"!
% ########################################################################### %

%% Error checking on inpu
if size(x,1) ~= size(y,1)
    error('nwRegress.m: Unequal number of observations in y and x');
end

if (nargin < 4)
    error('nwRegress.m: Not enough input parameters')
end

if (nargin > 4)
    error('nwRegress.m: Too many input parameters');
end

% Adding constant to X matrix if constant == 1
if constant == 1
    x = [ones(size(x,1),1) x];
end

%% Model estimation
% ########################################################################### %
%{
    We first compute estimates of the coefficients using the standard OLS
    estimator. We then compute fitted values, residuals, and the R-squared. 
    We then turn to the estimation of the standar errors using Newey-West. 
%}
% ########################################################################### %

% Setting preliminaries
[nObs,nReg] = size(y);
nVars       = size(x,2);

% Computing coefficient estimates
bv          = x\y;

% Computing input for standard errors
Exx         = x'*x/nObs;
errv        = y - x*bv;

% Computing coefficient of determination
s2          = sum(errv.^2)/nObs;
vary        = mean((y - ones(nObs,1) * mean(y)).^2);
R2v         = 100.*(1 - s2./vary)';
R2vadj      = 100.*(1 - (s2./vary) * (nObs-1)/(nObs-nVars))';

%% Computing Newey-West standard errors
% ########################################################################### %
%{
    We compute standard errors using the Newey-West (1987) estimator with
    a user-specified bandwith (number of lags). 
%}
% ########################################################################### %

% Preallocations
sbv     = zeros(nVars,nReg);
tbv     = zeros(nVars,nReg);

% Running individual regressions for each dependent variable
for iReg = 1:nReg

    ww      = 1;
    err     = errv(:,iReg);
    inner   = (x.*(err*ones(1,nVars)))' * (x.*(err*ones(1,nVars))) / nObs;

    % Looping over number of lags
    for iLag = 1:nlag

        innadd  = (x(1:nObs-iLag,:).*(err(1:nObs-iLag)*ones(1,nVars)))'*...
                (x(1+iLag:nObs,:).*(err(1+iLag:nObs)*ones(1,nVars)))/nObs;
        inner   = inner + (1-ww*iLag/(nlag+1))*(innadd+innadd');

    end

    % Computing the covariance matrix
    varb = Exx\inner/Exx/nObs;

    % Computing standard errors
    sbv(:,iReg) = sqrt(diag(varb));

    % Computing t-statistics
    tbv(:,iReg) = bv(:,iReg)./sbv(:,iReg);

end

% Creating structure for results
regResults.bv       = bv;
regResults.sbv      = sbv;
regResults.tbv      = tbv;
regResults.R2v      = R2v;
regResults.R2vadj   = R2vadj;
regResults.resid    = errv;

end

% ########################################################################### %
% [EOF]
% ########################################################################### %