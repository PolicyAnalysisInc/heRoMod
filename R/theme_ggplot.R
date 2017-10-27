#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
#*
#* This program is free software: you can redistribute it and/or modify
#* it under the terms of the GNU General Public License as published by
#* the Free Software Foundation, either version 3 of the License, or
#* (at your option) any later version.
#*
#* This program is distributed in the hope that it will be useful,
#* but WITHOUT ANY WARRANTY; without even the implied warranty of
#* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#* GNU General Public License for more details.
#*
#* You should have received a copy of the GNU General Public License
#* along with this program.  If not, see <http://www.gnu.org/licenses/>.
#**************************************************************************


# from Corson N. Areshenkoff
# http://areshenk-research-notes.com/publication-quality-plots-in-r/

theme_pub_bw <- function (base_size = 12, base_family = "") {
  
  ggplot2::theme_grey(base_size = base_size, 
                      base_family = base_family) %+replace% 
    
    ggplot2::theme(# Set text size
      plot.title = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size = 16),
      axis.title.y = ggplot2::element_text(size = 16, 
                                           angle = 90),
      
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      
      strip.text.x = ggplot2::element_text(size = 15),
      strip.text.y = ggplot2::element_text(size = 15,
                                           angle = -90),
      
      # Legend text
      legend.title = ggplot2::element_text(size = 15),
      legend.text = ggplot2::element_text(size = 15),
      
      # Configure lines and axes
      axis.ticks.x = ggplot2::element_line(colour = "black"), 
      axis.ticks.y = ggplot2::element_line(colour = "black"), 
      
      # Plot background
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey83", 
                                               size = 0.2), 
      panel.grid.minor = ggplot2::element_line(colour = "grey88", 
                                               size = 0.5), 
      
      # Facet labels        
      legend.key = ggplot2::element_rect(colour = "grey80"), 
      strip.background = ggplot2::element_rect(fill = "grey80", 
                                               colour = "grey50", 
                                               size = 0.2))
}
