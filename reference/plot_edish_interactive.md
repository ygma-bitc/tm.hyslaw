# Interactive liver-injury screening scatter (eDISH / mDISH / cholestatic)

Builds an interactive `plotly` scatter plot sharing the same
classification and axis rules as
[`plot_edish()`](https://ygma-bitc.github.io/tm.hyslaw/reference/plot_edish.md),
with subject-level hover tooltips showing peak values, peak dates,
directed peak gaps (`BILI - ALT/AST` or `BILI - ALP`), concurrent
status, ALP window/missing flags, and R-ratio.

## Usage

``` r
plot_edish_interactive(
  adsl = NULL,
  adlb = NULL,
  classified = NULL,
  which = c("edish", "mdish", "cholestatic"),
  color_by = c("arm", "quadrant"),
  log_axes = TRUE,
  facet = FALSE,
  annotate_quadrants = TRUE,
  mark_potential = TRUE,
  title = NULL,
  show_caption = TRUE,
  ...
)
```

## Arguments

- adsl, adlb:

  Passed to
  [`classify_hyslaw()`](https://ygma-bitc.github.io/tm.hyslaw/reference/classify_hyslaw.md)
  when `classified` is `NULL`.

- classified:

  Optional list from
  [`classify_hyslaw()`](https://ygma-bitc.github.io/tm.hyslaw/reference/classify_hyslaw.md).

- which:

  `"edish"` is peak ALT or AST / ULN vs peak BILI / ULN; `"mdish"` uses
  peak / baseline; `"cholestatic"` is peak ALP / ULN vs peak BILI / ULN.

- color_by:

  `"arm"` colors points by the grouping variable (default, for treatment
  imbalance). `"quadrant"` colors by screening quadrant.

- log_axes:

  If `TRUE`, both axes use log10 scales.

- facet:

  If `TRUE`, facet by the grouping variable when it has at least two
  levels. Default `FALSE` keeps arms on one panel.

- annotate_quadrants:

  If `TRUE`, print quadrant names in the corners.

- mark_potential:

  If `TRUE`, overlay open red circles on biochemical potential Hy's Law
  (eDISH / mDISH) or concurrent cholestatic cases.

- title:

  Plot title. Generated from `which` when `NULL`.

- show_caption:

  If `TRUE` (default), render the screening note as a bottom plot
  annotation with extra bottom margin. Set to `FALSE` when the caller
  renders the caption outside the widget (as in
  [`tm_g_edish()`](https://ygma-bitc.github.io/tm.hyslaw/reference/tm_g_edish.md)).

- ...:

  Passed to
  [`classify_hyslaw()`](https://ygma-bitc.github.io/tm.hyslaw/reference/classify_hyslaw.md).

## Value

A `plotly` (`htmlwidget`) object.

## Details

Red open circles for biochemical potential / concurrent cases skip hover
so each subject produces a single tooltip. Quadrant corner labels are
placed as non-interactive layout annotations.

## Examples

``` r
data <- make_synthetic_hyslaw_data(n_extra = 0L, seed = 1L)
plot_edish_interactive(data$ADSL, data$ADLB)

{"x":{"visdat":{"1a246846d40a":["function () ","plotlyVisDat"],"1a2457e41d1d":["function () ","data"],"1a24433775bc":["function () ","data"],"1a244c3e9fc0":["function () ","data"]},"cur_data":"1a244c3e9fc0","attrs":{"1a2457e41d1d":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"markers","name":"Investigational","legendgroup":"Investigational","showlegend":true,"text":{},"hoverinfo":"text","marker":{"size":8,"opacity":0.84999999999999998,"color":"#F8766D"},"inherit":true},"1a24433775bc":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"markers","name":"Placebo","legendgroup":"Placebo","showlegend":true,"text":{},"hoverinfo":"text","marker":{"size":8,"opacity":0.84999999999999998,"color":"#00BFC4"},"inherit":true},"1a244c3e9fc0":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"markers","showlegend":false,"hoverinfo":"skip","marker":{"symbol":"circle-open","size":14,"color":"red","line":{"width":2,"color":"red"}},"inherit":true}},"layout":{"margin":{"b":150,"l":65,"t":50,"r":20},"title":{"text":"eDISH (peak ALT or AST / ULN vs peak BILI / ULN)","x":0,"xanchor":"left","font":{"size":15}},"xaxis":{"domain":[0,1],"automargin":true,"title":{"text":"Peak ALT or AST / ULN"},"type":"log","range":[-1,1],"tickmode":"array","tickvals":[0.10000000000000001,1,10],"ticktext":["0.1","1","10"],"gridcolor":"#EBEBEB","linecolor":"#CCCCCC","mirror":true,"zeroline":false},"yaxis":{"domain":[0,1],"automargin":true,"title":{"text":"Peak BILI / ULN"},"type":"log","range":[-1,1],"tickmode":"array","tickvals":[0.10000000000000001,1,10],"ticktext":["0.1","1","10"],"gridcolor":"#EBEBEB","linecolor":"#CCCCCC","mirror":true,"zeroline":false},"legend":{"title":{"text":"TRT01A"}},"shapes":[{"type":"line","x0":3,"x1":3,"y0":0,"y1":1,"xref":"x","yref":"y domain","line":{"dash":"dash","color":"#333333","width":1.2}},{"type":"line","x0":0,"x1":1,"y0":2,"y1":2,"xref":"x domain","yref":"y","line":{"dash":"dash","color":"#333333","width":1.2}}],"annotations":[{"text":"<i>Cholestasis<\/i>","x":0.02,"y":0.96999999999999997,"xref":"x domain","yref":"y domain","xanchor":"left","yanchor":"top","showarrow":false,"font":{"size":11,"color":"#595959"}},{"text":"<i>Hy's Law<\/i>","x":0.97999999999999998,"y":0.96999999999999997,"xref":"x domain","yref":"y domain","xanchor":"right","yanchor":"top","showarrow":false,"font":{"size":11,"color":"#595959"}},{"text":"<i>Normal or near normal<\/i>","x":0.02,"y":0.029999999999999999,"xref":"x domain","yref":"y domain","xanchor":"left","yanchor":"bottom","showarrow":false,"font":{"size":11,"color":"#595959"}},{"text":"<i>Temple's corollary<\/i>","x":0.97999999999999998,"y":0.029999999999999999,"xref":"x domain","yref":"y domain","xanchor":"right","yanchor":"bottom","showarrow":false,"font":{"size":11,"color":"#595959"}},{"text":"Dashed lines: x &gt;= 3 x ULN, y &gt;= 2 x ULN. Axis maxima are not required to occur on the same day.<br>On-treatment peaks (ONTRTFL = Y when present); baseline rows are used only for mDISH.<br>Red open circles: biochemical potential Hy's Law (bilirubin &gt;= 2x ULN on or within 14 days after ALT/AST &gt;= 3x ULN;<br>ALP &gt;= 2x ULN in the same window excludes the case). Peak Hy's Law color/quadrant is not the same rule.<br>Laboratory screening only; alternative causes are not assessed.","x":0,"y":-0.17999999999999999,"xref":"paper","yref":"paper","xanchor":"left","yanchor":"top","align":"left","showarrow":false,"font":{"size":11,"color":"#4D4D4D"}}],"hovermode":"closest","plot_bgcolor":"white","paper_bgcolor":"white","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[4.2000000000000002,4.5,3.7999999999999998,5,1.6000000000000001,1.2],"y":[2.3999999999999999,2.6000000000000001,2.2999999999999998,1.2,2.5,0.80000000000000004],"type":"scatter","mode":"markers","name":"Investigational","legendgroup":"Investigational","showlegend":true,"text":["<b>Subject:<\/b> I-HY-01 | <b>Group:<\/b> Investigational<br>Quadrant: Hy's Law<br>Peak ALT/AST: 4.20x ULN (ALT, 2024-01-11)<br>Peak BILI: 2.40x ULN (2024-01-17)<br>Peak gap (BILI - ALT/AST): +6 d (positive = BILI after ALT/AST)<br>Potential Hy's Law: Yes<br>ALP in window: Not elevated<br>R-ratio: 3.82 (Mixed)","<b>Subject:<\/b> I-PEAK-01 | <b>Group:<\/b> Investigational<br>Quadrant: Hy's Law<br>Peak ALT/AST: 4.50x ULN (ALT, 2024-01-09)<br>Peak BILI: 2.60x ULN (2024-02-10)<br>Peak gap (BILI - ALT/AST): +32 d (positive = BILI after ALT/AST)<br>Potential Hy's Law: No<br>R-ratio: 4.09 (Mixed)","<b>Subject:<\/b> I-ALP-01 | <b>Group:<\/b> Investigational<br>Quadrant: Hy's Law<br>Peak ALT/AST: 3.80x ULN (ALT, 2024-01-13)<br>Peak BILI: 2.30x ULN (2024-01-19)<br>Peak gap (BILI - ALT/AST): +6 d (positive = BILI after ALT/AST)<br>Potential Hy's Law: No (ALP excluded)<br>ALP in window: Elevated<br>R-ratio: 1.46 (Cholestatic)","<b>Subject:<\/b> I-TEMP-01 | <b>Group:<\/b> Investigational<br>Quadrant: Temple's corollary<br>Peak ALT/AST: 5.00x ULN (ALT, 2024-01-15)<br>Peak BILI: 1.20x ULN (2024-01-15)<br>Peak gap (BILI - ALT/AST): 0 d (positive = BILI after ALT/AST)<br>Potential Hy's Law: No<br>R-ratio: 4.55 (Mixed)","<b>Subject:<\/b> I-BILI-01 | <b>Group:<\/b> Investigational<br>Quadrant: Cholestasis<br>Peak ALT/AST: 1.60x ULN (ALT, 2024-01-21)<br>Peak BILI: 2.50x ULN (2024-01-21)<br>Peak gap (BILI - ALT/AST): 0 d (positive = BILI after ALT/AST)<br>Potential Hy's Law: No<br>R-ratio: 1.60 (Cholestatic)","<b>Subject:<\/b> I-NORM-01 | <b>Group:<\/b> Investigational<br>Quadrant: Normal or near normal<br>Peak ALT/AST: 1.20x ULN (ALT, 2024-01-22)<br>Peak BILI: 0.80x ULN (2024-01-22)<br>Peak gap (BILI - ALT/AST): 0 d (positive = BILI after ALT/AST)<br>Potential Hy's Law: No<br>R-ratio: 1.33 (Cholestatic)"],"hoverinfo":["text","text","text","text","text","text"],"marker":{"color":"#F8766D","size":8,"opacity":0.84999999999999998,"line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[4.2000000000000002,5,1.6000000000000001,1.2],"y":[2.3999999999999999,1.2,2.5,0.80000000000000004],"type":"scatter","mode":"markers","name":"Placebo","legendgroup":"Placebo","showlegend":true,"text":["<b>Subject:<\/b> P-HY-01 | <b>Group:<\/b> Placebo<br>Quadrant: Hy's Law<br>Peak ALT/AST: 4.20x ULN (ALT, 2024-01-11)<br>Peak BILI: 2.40x ULN (2024-01-17)<br>Peak gap (BILI - ALT/AST): +6 d (positive = BILI after ALT/AST)<br>Potential Hy's Law: Yes<br>ALP in window: Not elevated<br>R-ratio: 3.82 (Mixed)","<b>Subject:<\/b> P-TEMP-01 | <b>Group:<\/b> Placebo<br>Quadrant: Temple's corollary<br>Peak ALT/AST: 5.00x ULN (ALT, 2024-01-15)<br>Peak BILI: 1.20x ULN (2024-01-15)<br>Peak gap (BILI - ALT/AST): 0 d (positive = BILI after ALT/AST)<br>Potential Hy's Law: No<br>R-ratio: 4.55 (Mixed)","<b>Subject:<\/b> P-BILI-01 | <b>Group:<\/b> Placebo<br>Quadrant: Cholestasis<br>Peak ALT/AST: 1.60x ULN (ALT, 2024-01-21)<br>Peak BILI: 2.50x ULN (2024-01-21)<br>Peak gap (BILI - ALT/AST): 0 d (positive = BILI after ALT/AST)<br>Potential Hy's Law: No<br>R-ratio: 1.60 (Cholestatic)","<b>Subject:<\/b> P-NORM-01 | <b>Group:<\/b> Placebo<br>Quadrant: Normal or near normal<br>Peak ALT/AST: 1.20x ULN (ALT, 2024-01-22)<br>Peak BILI: 0.80x ULN (2024-01-22)<br>Peak gap (BILI - ALT/AST): 0 d (positive = BILI after ALT/AST)<br>Potential Hy's Law: No<br>R-ratio: 1.33 (Cholestatic)"],"hoverinfo":["text","text","text","text"],"marker":{"color":"#00BFC4","size":8,"opacity":0.84999999999999998,"line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[4.2000000000000002,4.2000000000000002],"y":[2.3999999999999999,2.3999999999999999],"type":"scatter","mode":"markers","showlegend":false,"hoverinfo":["skip","skip"],"marker":{"color":"red","symbol":"circle-open","size":14,"line":{"color":"red","width":2}},"error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"line":{"color":"rgba(44,160,44,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}plot_edish_interactive(data$ADSL, data$ADLB, which = "cholestatic")

{"x":{"visdat":{"1a244727723e":["function () ","plotlyVisDat"],"1a2458ff1318":["function () ","data"],"1a245653636c":["function () ","data"],"1a2435668128":["function () ","data"]},"cur_data":"1a2435668128","attrs":{"1a2458ff1318":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"markers","name":"Investigational","legendgroup":"Investigational","showlegend":true,"text":{},"hoverinfo":"text","marker":{"size":8,"opacity":0.84999999999999998,"color":"#F8766D"},"inherit":true},"1a245653636c":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"markers","name":"Placebo","legendgroup":"Placebo","showlegend":true,"text":{},"hoverinfo":"text","marker":{"size":8,"opacity":0.84999999999999998,"color":"#00BFC4"},"inherit":true},"1a2435668128":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"markers","showlegend":false,"hoverinfo":"skip","marker":{"symbol":"circle-open","size":14,"color":"red","line":{"width":2,"color":"red"}},"inherit":true}},"layout":{"margin":{"b":150,"l":65,"t":50,"r":20},"title":{"text":"Cholestatic screening (peak ALP / ULN vs peak BILI / ULN)","x":0,"xanchor":"left","font":{"size":15}},"xaxis":{"domain":[0,1],"automargin":true,"title":{"text":"Peak ALP / ULN"},"type":"log","range":[-1,1],"tickmode":"array","tickvals":[0.10000000000000001,1,10],"ticktext":["0.1","1","10"],"gridcolor":"#EBEBEB","linecolor":"#CCCCCC","mirror":true,"zeroline":false},"yaxis":{"domain":[0,1],"automargin":true,"title":{"text":"Peak BILI / ULN"},"type":"log","range":[-1,1],"tickmode":"array","tickvals":[0.10000000000000001,1,10],"ticktext":["0.1","1","10"],"gridcolor":"#EBEBEB","linecolor":"#CCCCCC","mirror":true,"zeroline":false},"legend":{"title":{"text":"TRT01A"}},"shapes":[{"type":"line","x0":2,"x1":2,"y0":0,"y1":1,"xref":"x","yref":"y domain","line":{"dash":"dash","color":"#333333","width":1.2}},{"type":"line","x0":0,"x1":1,"y0":2,"y1":2,"xref":"x domain","yref":"y","line":{"dash":"dash","color":"#333333","width":1.2}}],"annotations":[{"text":"<i>Isolated BILI<\/i>","x":0.02,"y":0.96999999999999997,"xref":"x domain","yref":"y domain","xanchor":"left","yanchor":"top","showarrow":false,"font":{"size":11,"color":"#595959"}},{"text":"<i>ALP + BILI<\/i>","x":0.97999999999999998,"y":0.96999999999999997,"xref":"x domain","yref":"y domain","xanchor":"right","yanchor":"top","showarrow":false,"font":{"size":11,"color":"#595959"}},{"text":"<i>Normal<\/i>","x":0.02,"y":0.029999999999999999,"xref":"x domain","yref":"y domain","xanchor":"left","yanchor":"bottom","showarrow":false,"font":{"size":11,"color":"#595959"}},{"text":"<i>Isolated ALP<\/i>","x":0.97999999999999998,"y":0.029999999999999999,"xref":"x domain","yref":"y domain","xanchor":"right","yanchor":"bottom","showarrow":false,"font":{"size":11,"color":"#595959"}},{"text":"Dashed lines: x &gt;= 2 x ULN, y &gt;= 2 x ULN. Axis maxima are not required to occur on the same day.<br>On-treatment peaks (ONTRTFL = Y when present); baseline rows are used only for mDISH.<br>Red open circles: bilirubin &gt;= 2x ULN on or within 14 days after ALP &gt;= 2x ULN.<br>Laboratory screening only; alternative causes are not assessed.","x":0,"y":-0.17999999999999999,"xref":"paper","yref":"paper","xanchor":"left","yanchor":"top","align":"left","showarrow":false,"font":{"size":11,"color":"#4D4D4D"}}],"hovermode":"closest","plot_bgcolor":"white","paper_bgcolor":"white","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[1.2,1.1000000000000001,2.6000000000000001,1.1000000000000001,1,0.90000000000000002],"y":[2.3999999999999999,2.6000000000000001,2.2999999999999998,1.2,2.5,0.80000000000000004],"type":"scatter","mode":"markers","name":"Investigational","legendgroup":"Investigational","showlegend":true,"text":["<b>Subject:<\/b> I-HY-01 | <b>Group:<\/b> Investigational<br>Quadrant: Isolated hyperbilirubinemia<br>Peak ALP: 1.20x ULN (2024-01-17)<br>Peak BILI: 2.40x ULN (2024-01-17)<br>Peak gap (BILI - ALP): 0 d (positive = BILI after ALP)<br>Concurrent cholestatic: No<br>R-ratio: 3.82 (Mixed)","<b>Subject:<\/b> I-PEAK-01 | <b>Group:<\/b> Investigational<br>Quadrant: Isolated hyperbilirubinemia<br>Peak ALP: 1.10x ULN (2024-02-10)<br>Peak BILI: 2.60x ULN (2024-02-10)<br>Peak gap (BILI - ALP): 0 d (positive = BILI after ALP)<br>Concurrent cholestatic: No<br>R-ratio: 4.09 (Mixed)","<b>Subject:<\/b> I-ALP-01 | <b>Group:<\/b> Investigational<br>Quadrant: ALP and BILI elevated<br>Peak ALP: 2.60x ULN (2024-01-13)<br>Peak BILI: 2.30x ULN (2024-01-19)<br>Peak gap (BILI - ALP): +6 d (positive = BILI after ALP)<br>Concurrent cholestatic: Yes<br>R-ratio: 1.46 (Cholestatic)","<b>Subject:<\/b> I-TEMP-01 | <b>Group:<\/b> Investigational<br>Quadrant: Normal or near normal<br>Peak ALP: 1.10x ULN (2024-01-15)<br>Peak BILI: 1.20x ULN (2024-01-15)<br>Peak gap (BILI - ALP): 0 d (positive = BILI after ALP)<br>Concurrent cholestatic: No<br>R-ratio: 4.55 (Mixed)","<b>Subject:<\/b> I-BILI-01 | <b>Group:<\/b> Investigational<br>Quadrant: Isolated hyperbilirubinemia<br>Peak ALP: 1.00x ULN (2024-01-21)<br>Peak BILI: 2.50x ULN (2024-01-21)<br>Peak gap (BILI - ALP): 0 d (positive = BILI after ALP)<br>Concurrent cholestatic: No<br>R-ratio: 1.60 (Cholestatic)","<b>Subject:<\/b> I-NORM-01 | <b>Group:<\/b> Investigational<br>Quadrant: Normal or near normal<br>Peak ALP: 0.90x ULN (2024-01-22)<br>Peak BILI: 0.80x ULN (2024-01-22)<br>Peak gap (BILI - ALP): 0 d (positive = BILI after ALP)<br>Concurrent cholestatic: No<br>R-ratio: 1.33 (Cholestatic)"],"hoverinfo":["text","text","text","text","text","text"],"marker":{"color":"#F8766D","size":8,"opacity":0.84999999999999998,"line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1.2,1.1000000000000001,1,0.90000000000000002],"y":[2.3999999999999999,1.2,2.5,0.80000000000000004],"type":"scatter","mode":"markers","name":"Placebo","legendgroup":"Placebo","showlegend":true,"text":["<b>Subject:<\/b> P-HY-01 | <b>Group:<\/b> Placebo<br>Quadrant: Isolated hyperbilirubinemia<br>Peak ALP: 1.20x ULN (2024-01-17)<br>Peak BILI: 2.40x ULN (2024-01-17)<br>Peak gap (BILI - ALP): 0 d (positive = BILI after ALP)<br>Concurrent cholestatic: No<br>R-ratio: 3.82 (Mixed)","<b>Subject:<\/b> P-TEMP-01 | <b>Group:<\/b> Placebo<br>Quadrant: Normal or near normal<br>Peak ALP: 1.10x ULN (2024-01-15)<br>Peak BILI: 1.20x ULN (2024-01-15)<br>Peak gap (BILI - ALP): 0 d (positive = BILI after ALP)<br>Concurrent cholestatic: No<br>R-ratio: 4.55 (Mixed)","<b>Subject:<\/b> P-BILI-01 | <b>Group:<\/b> Placebo<br>Quadrant: Isolated hyperbilirubinemia<br>Peak ALP: 1.00x ULN (2024-01-21)<br>Peak BILI: 2.50x ULN (2024-01-21)<br>Peak gap (BILI - ALP): 0 d (positive = BILI after ALP)<br>Concurrent cholestatic: No<br>R-ratio: 1.60 (Cholestatic)","<b>Subject:<\/b> P-NORM-01 | <b>Group:<\/b> Placebo<br>Quadrant: Normal or near normal<br>Peak ALP: 0.90x ULN (2024-01-22)<br>Peak BILI: 0.80x ULN (2024-01-22)<br>Peak gap (BILI - ALP): 0 d (positive = BILI after ALP)<br>Concurrent cholestatic: No<br>R-ratio: 1.33 (Cholestatic)"],"hoverinfo":["text","text","text","text"],"marker":{"color":"#00BFC4","size":8,"opacity":0.84999999999999998,"line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2.6000000000000001],"y":[2.2999999999999998],"type":"scatter","mode":"markers","showlegend":false,"hoverinfo":"skip","marker":{"color":"red","symbol":"circle-open","size":14,"line":{"color":"red","width":2}},"error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"line":{"color":"rgba(44,160,44,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
