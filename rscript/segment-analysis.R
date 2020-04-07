install.packages("segmented")
library(segmented)

# https://lindeloev.github.io/mcp/articles/packages.html
#
# test 1-10 breakpoints and find optimal # with BIC
# see if september 2018 is identified as a significant break point.

fit_glm <- glm(sl ~ 1+ date, data = np )

fit_segmented_1 = segmented(fit_glm, seg.Z = ~x, npsi = 1)
fit_segmented_1 = segmented(fit_lm, seg.Z = ~x, npsi = 5)


BF = exp((BIC(fit_segmented) - BIC(fit_segmented_1))/2)  # From Wagenmakers (2007)
BF