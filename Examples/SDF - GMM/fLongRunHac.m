function [S] = fLongRunHac(GT,flagAndrews,varargin)
% This function computes the long-run covariance matrix S, possibly with a
% HAC correction. If nLags = 0, we use the sample covariance matrix. If
% nLags > 0, we do a HAC correction, using nLags as bandwidth in the
% Bartlett weight. 
%
% Input: 
%   GT          = time series of sample moments 
%   flagAndrews = if equal to 1, we do optimal bandwidth estimation and 
%                 any input in varargin is irellevant. 
%                 if equal to 0, we use the user choosen bandwidth in
%                 varargin and define nLags as this value. 
% 
%% Check 

if flagAndrews == 0
    if (nargin < 3)
        error('fLongRunHac.m: You need to chose a bandwidth length');
    end 
end

%% Preliminaries
[T,K]      = size(GT);

% Get Gbar 
Gbar       = mean(GT,1); 

%% Compute first S as sample covariance
GdeMean    = GT - repmat(Gbar,T,1);
Gamma      = T^(-1)*(GdeMean)'*(GdeMean);
S          = Gamma;

%% Compute optimal bandwidth via Andrews (1991)

if flagAndrews == 1 % compute the optimal bandwidth
    iota            = ones(T-1,1); 

    for iElem = 1:K
        % Estimating AR(1) coefficient for each components 
        rho             = [iota GT(1:end-1,iElem)]\GT(2:end,iElem);
        sigma2          = mean(([iota GT(1:end-1,iElem)]*rho - GT(2:end,iElem)).^2);
        alphaN(iElem)   = (4*rho(2)^2 * sigma2^2 / ((1-rho(2))^6 * (1+rho(2))^2));
        alphaD(iElem)   = (sigma2^2 / (1-rho(2))^4);
    end

    % Estimate alpha as equal-weighted average as in Andrews (1991)
    alphaParm   = sum(alphaN)/sum(alphaD);

    % Estimating the Bartlett specific bandwidth parameter (ceil to get even number, conservative)
    nLags       = ceil( 1.1447*(alphaParm*T)^(1/3) );
elseif flagAndrews == 0 % pick the user selected one
    nLags       = varargin{1};
end

%% Add HAC correction (if nLags > 0)
if nLags > 0 
   for j=1:nLags
      Gamma     = T^(-1)*(GdeMean(j+1:end,:)'*GdeMean(1:end-j,:));
      weight    = (1-j/(nLags+1)); %Bartlett type of weights
      S         = S + weight.*(Gamma + Gamma');
   end 
end
     
      
end