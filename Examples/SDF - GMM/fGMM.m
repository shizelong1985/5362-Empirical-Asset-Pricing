function [output] = fGMM(param0,ret,rf,cons,z,flagAndrews,nLags,nIter)
% This function conducts GMM estimation, either first, second stage or 
% iterated stages. In the first stage, the identity matrix is employed as 
% weighting matrix. In the second and subsequent stages, the optimal
% weighting matrix (S^(-1)) is used. You can choose to stop the iterations
% at first stage by indicating so via the % variable nIter: if nIter = 1,
% we conduct first stage. For nIter = M, one conducts M-stage GMM. 

% V denotes the covariance matrix of parameters, and Omega denotes the
% covariance matrix of sample moments, gT.

% nLags determines the length of the bandwidth in the HAC estimator for the
% long-run covariance matrix S. 

% nIter determines whether it is one-stage, two-stage or iterated GMM,
% where the first stage uses the weighting matrix equal to the identity
% matrix and the remaining stages used the optimal S^(-1). Note that, as a
% result, if nIter you run GMM with the fixed, identity matrix as
% weighting matrix.

% Note that this function is specific to the CCAPM, and the moments below 
% should be changed.

% Dimensions 
[~,GT]     = fMoments_CCAPM(param0,ret,rf,cons,z);
[T,N]      = size(GT);
q          = size(param0,1);

% Fix weighting matrix in first stage as identity matrix
AT         = eye(N);

% Constraints and options in fmincon used below
lb     = [eps eps];
ub     = [1 inf];

% We tell the function that we want diagnostics off, we do not want to get
% all the output of optimization printed out and we want to use the
% interior point algorithm for optimization
options = optimset('Diagnostics','off','Display','none','Algorithm','interior-point');

% We can also run it unconstrained using the functions below, but you will
% need to adap the code yourself using those. 
% fminunc(@(param) fGMM_obj(param,ret,rf,cons,z,AT),param0,options)
% fminsearch(@(param) fGMM_obj(param,ret,rf,cons,z,AT),param0,options)

    
i = 1;
while i <= nIter 

    % For additional iteration, redefine initial value to current stage
    % estimate and weighting matrix to optimal one
    if i > 1
        param0 = param;
        AT     = inv(S);
    end
    
    % Numerical optimization, minimizing the GMM objetive function (Q)     
    [param] = ...
    fmincon(@fGMM_obj,param0,[],[],[],[],lb,ub,[],options,ret,rf,cons,z,AT);
    disp(strcat('Parameters after optimization stage: ',num2str(i)));
    param

    % Get GT at optimum 
    [gT,GT]     = fMoments_CCAPM(param,ret,rf,cons,z);

    % Compute long-run covar
    S = fLongRunHac(GT,flagAndrews,nLags);

    i=i+1;
end

% Compute gradient
D      = fGradient(param,ret,rf,cons,z); 

% Compute covariance matrices
V         = inv(D'*AT*D)*D'*AT*S*AT*D*inv(D'*AT*D);
Omega     = (eye(N) - D*inv(D'*AT*D)*D'*AT)*S*(eye(N) - AT*D*inv(D'*AT*D)*D');

covTheta  = 1/T*V;
covOmega  = 1/T*Omega;

% Compute output 
stdErrTheta    = sqrt(diag(covTheta));
JT             = gT*pinv(covOmega)*gT';

% Gather output
output.theta       = param; 
output.stdErr      = stdErrTheta'; 
output.tStat       = param./stdErrTheta'; 
output.S           = S; 
output.covTheta    = covTheta; 
output.covOmega    = covOmega;
output.J           = JT;
output.Jpval       = 1 - chi2cdf(JT,N-q);

end