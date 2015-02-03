% Applications of Convolution
%
% Part 1: Filtering Data, 2P imaging of neuronal calcium 
%
% As we saw in lab01, multiple noise sources can be present in data
% recordings and must be removed to better detect the signal.  In this
% section, we will look at data collected using 2-photon imaging from a
% calcium indicator dye loaded into a murine brain slice. When a neuron
% fires an action potential, there is a large and rapid influx of calcium
% into the cell. Using a fluorophore that binds to calcium, we can use this
% phenomenon to observe activity in large populations of neurons. The
% measured signal is actually a count of photons emitted from the
% fluorophore, and represents the change in fluorescence in an individual
% neuron. This change in fluorescence represents the concentration of
% calcium in the cell, which reflects action potential activity (or
% _firing_). Every time the neuron spikes, there is a quick rise in the
% fluorescence signal followed by exponential decay. The time the spike
% occurs is at the initial rise from baseline, NOT the peak of the signal.
% 
% This method of signal detection is sensitive to Johnson and Shot noise,
% which are both Gaussian in their distribution (assuming large numbers).
% While Gaussian white noise (GWN) is annoyingly omnipresent, it can be
% conveniently removed by taking the average of many signals. This concept
% is used often in scientific research with a large enough sample size, so 
% the noise can be averaged out.
% 
% Many other types of noise, however, cannot averaged out, and there are a
% wide variety of filters that exist for this reason. Periodic noise is one
% such example. Periodic noise is often caused by instrumentation, and can
% be removed with a _notch_ ak.k. _stopband_ filter if it occurs at one
% specific frequency. alternatively, a passband filter can be used to
% remove frequencies abover (lowpass) or below (highpass) a set cutoff. One
% common filter is the butterworth filter. Filter types vary in how the the
% steepness of their rolloff, that is, how sharply they cut off the
% specified filter frequencies. Let's use as an example a signal with two
% frequesncies present and some GWN. As is the instance with all signal
% processing, your nyquist frequency, which depends on your sampling rate,
% is very important in your analyses.

fs = 1000;                          % sampling frequency   
nyquist = fs/2;                     % nyquist frequency
nfft = 512;                         % number of sampled points (best if this is a power of 2)
range = (nfft/2)+1;                 % set the range for the spectral plot
t = 0:1/fs:0.6;                     % set time vector
f = fs*(0:range-1)/nfft;            % set frequency vector
x = sin(2*pi*50*t)+sin(2*pi*120*t); % make a series with 50,120 Hz signals
y = x+randn(1,length(t));           % add gaussian noise

Y = fft(y, nfft);                   % compute FFT
Pyy = 2*Y.*conj(Y) /nfft;           % compute power spectrum
figure;
plot(f, Pyy(1:range)); 
xlabel('frequency (Hz)');ylabel('power (mV^2)');

% Now, let's create a lowpass filter with a cutoff frequency of 100Hz.

[b,a] = butter(6, 100/nyquist,'low');

% Notice the first input into the butterworth filter function is a _6_.
% This is the order of the filter, which determines the steepness off the
% rolloff. Try using a first order filter and see how this affects the
% filtered signal.
%
% The _butter_ command in MATLAB only creates the filter. To filter your
% signal, you must pass your filter coefficients (b,a) to a filter
% function, like this:

filteredData = filter(b,a,y);

% To view the results, plot your power spectra again after the signal has been
% filtered:

Y=fft(filteredData, nfft); % compute FFT
Pyy = 2*Y.*conj(Y) /nfft;  % compute power spectrum
figure;
plot(f, Pyy(1:range)); 
xlabel('frequency (Hz)');ylabel('power (mV^2)');

% Load the file 'fluoData.mat.' This file contains 100 rows
% corresponding to 100 cells, the first 50 belonging to one population of
% cells, the second 50 belonging to another. The values on the Y axis are
% fluorescence values in arbitrary units.  The X axis is time, where each
% sample is from a 20ms long frame (corresponding to a sample rate of
% 50Hz).  The X axis should have 2750 data points, corresponding to 55
% seconds of total imaging. To plot the fluorescence value for neuron #1,
%
% For the sake of this problem, you can assume that all cells of a single
% population exhibit synchronous firing.  Therefore if one cell fires, all
% cells will fire in the same 20ms frame.  The same goes with the other
% population.  In this question we are going to ask, which population of
% cells is firing first?
%
% Calculate the power spectra of the noise in the data, by taking the Fourier
% transform of a time window in which there is no real signal. 

load('fluoData.mat')
y = fluoData(45:55,1:500);
fs = 50;                   % sampling frequency   
nyquist = fs/2 ;           % nyquist frequency
nfft = 2750;               % number of sampled points (best if this is a power of 2)
range = (nfft/2)+1;        % set the range for the spectral plot
t = 0:1/fs:0.6;            % set time vector
f = fs*(0:range-1)/nfft;   % set frequency vector
Y = fft(y, nfft);          % compute FFT
Pyy = 2*Y.*conj(Y) /nfft;  % compute power spectrum
figure;
plot(f, Pyy(1:range)); 
xlabel('frequency (Hz)');ylabel('power (mV^2)');
 
% Use the butterworth filter to remove frequencies in the noise 
% spectrum from the signal. Plot an example neuron from
% each population in fluorescence (a.u.) vs time (ms) before and after
% filtering.

load('fluoData.mat')
y1 = fluoData(25,1:2750);
y2 = fluoData(75,1:2750);
fs = 50;                  % sampling frequency   
nyquist = fs/2;           % nyquist frequency
nfft = 2750;              % number of sampled points (best if this is a power of 2)
range = (nfft/2)+1;       % set the range for the spectral plot
t = 0:1/fs:0.6;           % set time vector
f = fs*(0:range-1)/nfft;  % set frequency vector
[b,a] = butter(6, 5/nyquist,'low');
filteredData1 = filter(b,a,y1);
filteredData2 = filter(b,a,y2);
figure;
plot(y1, 'c');
hold on;
plot(filteredData1, 'm');
legend('Pre-filter', 'Post-filter');
title('Neuron 25, Population 1');xlabel('Time (ms)');ylabel('Fluorescence (a.u.)');
figure;
plot(y2, 'y');
hold on;
plot(filteredData2, 'k');
legend('Pre-filter', 'Post-filter');
title('Neuron 75, Population 2');xlabel('Time (ms)');ylabel('Fluorescence (a.u.)');

% Do the two populations (neurons 1-50 and 51-100) fire at the same time?
% To answer this question, take the mean, or the sum of the 50 filtered
% signals in each group, and plot them on the same graph using diferent
% colors.

load('fluoData.mat')
y1 = fluoData(1:50,1:2750);
y3 = sum(y1);
y2 = fluoData(51:100,1:2750);
y4 = sum(y2);
fs = 50;                 % sampling frequency   
nyquist = fs/2;          % nyquist frequency
nfft = 2750;             % number of sampled points (best if this is a power of 2)
range = (nfft/2)+1;      % set the range for the spectral plot
t = 0:1/fs:0.6;          % set time vector
f = fs*(0:range-1)/nfft; % set frequency vector
[b,a] = butter(6, 5/nyquist,'low');
filteredData3 = filter(b,a,y3);
filteredData4 = filter(b,a,y4);
figure;
plot(filteredData3, 'c');
hold on;
plot(filteredData4, 'm');
legend('Population 1', 'Population 2');
title('Action Potentials of Populations 1 and 2');xlabel('Time (ms)');ylabel('Fluorescence (a.u.)');

% Part 2: Design your own filters.
%
% In this part you will design some simple convolution filters in the
% frequency domain and test whether they work on a signal. Use the mean or
% sum of the first 50 rows in the fluoroData set provided above as your
% data.
%
% Come up with a simple low-pass frequency filter by creating a vector
% of 1s and 0s in the frequency domain, multiplying it by the Fourier transform
% of the data, and taking the inverse FT of the product using ifft. Try
% different cutoff frequencies and compare the filtered output to that from the
% butterworth filter used in Part 1.

load('fluoData.mat')
y5 = fluoData(1:50,1:2750);
y6 = sum(y5);
fs = 50;                 % sampling frequency   
nyquist = fs/2;          % nyquist frequency
nfft = 2750;             % number of sampled points (best if this is a power of 2)
range = (nfft/2)+1;      % set the range for the spectral plot
t = 0:1/fs:0.6;          % set time vector
f = fs*(0:range-1)/nfft; % set frequency vector
Y = fft(y6, nfft);       % compute FFT
y = zeros(1,(length(Y)-1000));
a = [zeros(1,1000) y];

y1 = zeros(length(Y),1);
a1(1:10) = Y(1:10);

y2 = zeros(length(Y),1);
a2(1:5) = Y(1:5);

filtered1 = a.*Y
filtered2 = ifft(filtered1, nfft);
filtered4 = ifft(y1);
filtered6 = ifft(y2);

[b,a] = butter(6, 5/nyquist,'low');
filtered7 = filter(b,a,y6);
figure;
plot(filtered2, 'c'); 
hold on;
plot(filtered4, 'm');
hold on;
plot(filtered6, 'y');
hold on;
plot(filtered7, 'm');
legend('Filter - first 20 freq', 'Filter - first 10 freq', 'Filter - first 5 freq', 'Butterworth Filter - 5Hz');
title('Population 1 Frequency');xlabel('frequency (Hz)');ylabel('power (mV^2)');

% Part 3: Image Processing
%
% MATLAB has exellent built-in image processing functions which are hard to 
% replicate by hand, so in this part I will simply introduce you to some of them. 
%
% Use the filtering capabilities to mess up the peppers image, using commands
% fspecial and imfilter. (See the help page for fspecial for examples). In 
% particular, use the 'motion' and 'gaussian' filters to produce two messed
% up images from the image you loaded. Produce images of both.

pic = imread('peppers.png');
image(pic);

h = fspecial('gaussian', 50, 45);
filteredpic1 = imfilter(pic, h);
figure, image(filteredpic1);
title('Original Filtered Pic 1');
h1 = fspecial('motion', 50, 45);
filteredpic2 = imfilter(pic, h1);
figure, image(filteredpic2);
title('Original Filtered Pic 2');

% Use the deconvolution capabilities of deconvblind to see how well you
% can restore the original image (see the help page for deconvblind for
% examples). Try starting with different initial point spread functions
% (PSFs) - randing from something very simple like a 2 by 2 matrix of 1s,
% to exactly the PSF that you used to convolve the data in 3.1. Produce
% images of the reconstructed results.

INITPSF = [ 1 1; 1 1 ]
J = deconvblind(filteredpic1,INITPSF,30);
figure, image(J);
title('Restored Image');
K = deconvblind(filteredpic1,h,30);
figure, image(K);
title('Restored Image 2');
