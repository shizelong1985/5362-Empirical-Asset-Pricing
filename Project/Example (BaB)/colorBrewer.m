function brewedColors = colorBrewer(numColor)

%% colorBrewer.m
% ########################################################################### %
% function  brewedColors = colorBrewer(numColor)
% Purpose:  Create a vector of RGB colors for plotting purposes
%
% Input:    numColor    = Scalar indicating color to be used
%
% Output:   brewedColor = Matrix of RGB codes for colors for plots
%               
% Author:
% Jonas N. Eriksen
% Department of Economics and Business Economics
% Aarhus University and CREATES
%
% Encoding: UTF8
% Last modified: December, 2021
%
% NOTE: ONLY INTENDED FOR USE IN THE COURSE "EMPIRICAL ASSET PRICING"!
% ########################################################################### %

% Error checking on input
if (nargin > 1)
    error('colorBrewer.m: Too many input arguments');
end

if (nargin < 1)
    error('colorBrewer.m: Not enough input arguments');
end

if ~ismember(numColor,1:6)
    error('colorBrewer.m: A most five colors are currently supported');
end

%% Setting RGB values for color palette
% ########################################################################### %
%{
    Setting RGB values for the color palette used throughout plots. 
%}
% ########################################################################### %

% Setting matrix of RGB values
rgbValues = [
    66      133     244     % Blue
    219     68      55      % Red
    244     180     0       % Yellow
    15      157     88      % Green
    153     50      204     % Purple
    150     150     150     % Gray
]./255;

% Setting output
brewedColors  = rgbValues(numColor,:);

end

% ########################################################################### %
% [EOF]
% ########################################################################### %