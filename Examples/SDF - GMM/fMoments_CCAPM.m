function [gT,GT] = fMoments_CCAPM(param,ret,rf,cons,z)
% Function for estimation of the moments of the CCAPM, using
% the instruments (including a constant) in z.

% Define test asset excess returns and include the risk-free rate
excessRet   = ret - rf;
testAss     = [1 + rf excessRet];

% Get dimensions
[nObs,nAss]  = size(testAss);
nInst        = size(z,2);

% Set parameters
delta   = param(1);
rho     = param(2); 

% Define SDF 
sdf     = delta*cons.^(-rho);

% Using the compact notation (Kronecker formulation) for each time period 
GT      = NaN(nObs,nAss*nInst);

% Since we include the risk-free rate as another test assets, we need to 
%make sure we subtract one in the appropriate elements. 
correct    = [1 zeros(1,nAss-1)];

for t=1:nObs
    GT(t,:) = kron((sdf(t).*testAss(t,:)-correct),z(t,:));
end

% Get sample averages (sample moments) 
gT       = mean(GT);

end