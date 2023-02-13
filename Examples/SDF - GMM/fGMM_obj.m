function [Q] = fGMM_obj(param,ret,rf,cons,z,AT)
% This function computes the object we want to minimize in GMM. 

% Construct object to minimize (Q) 
[gT,~]     = fMoments_CCAPM(param,ret,rf,cons,z);
if size(gT,2) > size(gT,1)
    gT = gT';
end

Q  = gT'*AT*gT;

      
end