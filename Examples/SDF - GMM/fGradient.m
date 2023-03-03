function [D] = fGradient(param,ret,rf,cons,z)
% Function for estimating the gradient, D, of gT evaluated at param. 
% The function used forward gradients. 
% Note that this function is specific to the CCAPM, and the moment used in 
% generating g below should be changed.

% Get value of gT at GMM estimates 
[g0,~]     = fMoments_CCAPM(param,ret,rf,cons,z);
if size(g0,2) > size(g0,1)
    g0 = g0';
end

% Get sizes and ensure param is a column vector
[nMom,~]  = size(g0);
if size(param,2) > size(param,1)
    param = param';
end

% Number of parameters with respect to which we compute gradient
q      = size(param,1);

% Choose a (parameter-dependent, cf. below) small number for numerical increments 
h      = 1e-6; 

% Pre-allocate gradient
D      = zeros(nMom,q); 

% Compute numerical derivative for each parameter, deviating each parameter
% one by one
I       = eye(q); % helper matrix that allows us to pick each parameter in isolation
for j=1:q
    
    [g1,~]       = fMoments_CCAPM(param.*(ones(q,1) + I(:,j).*h),ret,rf,cons,z);
    if size(g1,2) > size(g1,1)
        g1 = g1';
    end
    D(:,j)       = (g1-g0)/(param(j).*h);
    
end

end