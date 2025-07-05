# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
sys.path.insert(0, os.path.abspath('../')) # Point to the project root for hero_py
sys.path.insert(0, os.path.abspath('../hero_py')) # Point directly to hero_py package


# -- Project information -----------------------------------------------------

project = 'hero_py'
copyright = '(c) 2023-2024, Project Contributors (Originally ported by Jules AI)' # Year needs update
author = 'Project Contributors'

# The full version, including alpha/beta/rc tags
# Attempt to get version from hero_py package
try:
    from hero_py import __version__ as version
except ImportError:
    version = '0.0.1.dev0' # Fallback

release = version


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.autodoc',       # Include documentation from docstrings
    'sphinx.ext.doctest',       # Test snippets in the documentation
    'sphinx.ext.intersphinx',   # Link to other projects’ documentation
    'sphinx.ext.todo',          # Support for todo items
    'sphinx.ext.coverage',      # Collect doc coverage stats
    'sphinx.ext.mathjax',       # Render math via MathJax
    'sphinx.ext.ifconfig',      # Conditional content
    'sphinx.ext.viewcode',      # Add links to source code
    'sphinx.ext.githubpages',   # Helps with GitHub Pages deployment
    'sphinx.ext.napoleon',      # Support for Google and NumPy style docstrings
    'myst_parser',              # For Markdown support
    'sphinx_autodoc_typehints', # Better type hint rendering
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# The master toctree document.
master_doc = 'index' # Sphinx 4+ uses 'root_doc', but 'master_doc' for compatibility
root_doc = 'index'   # For Sphinx 4+

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme' # ReadTheDocs theme is popular

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']
# html_logo = "_static/logo.png" # If you have a logo

# -- Options for autodoc -----------------------------------------------------
autodoc_member_order = 'bysource' # Keep source order for members
autodoc_default_options = {
    'members': True,
    'member-order': 'bysource',
    'undoc-members': False, # Usually True is better to show all, even without docstrings
    'show-inheritance': True,
}
# Napoleon settings
napoleon_google_docstring = True
napoleon_numpy_docstring = True # Set to False if you exclusively use Google style
napoleon_include_init_with_doc = True
napoleon_include_private_with_doc = False
napoleon_include_special_with_doc = True
napoleon_use_admonition_for_examples = False
napoleon_use_admonition_for_notes = False
napoleon_use_admonition_for_references = False
napoleon_use_ivar = False
napoleon_use_param = True
napoleon_use_rtype = True

# -- Options for intersphinx extension ---------------------------------------
# Example configuration for intersphinx: refer to the Python standard library.
intersphinx_mapping = {
    'python': ('https://docs.python.org/3', None),
    'numpy': ('https://numpy.org/doc/stable/', None),
    'pandas': ('https://pandas.pydata.org/pandas-docs/stable/', None),
    'scipy': ('https://docs.scipy.org/doc/scipy/', None),
    'matplotlib': ('https://matplotlib.org/stable/', None),
    'sklearn': ('https://scikit-learn.org/stable/', None),
}

# -- Options for todo extension ----------------------------------------------
# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = True

# -- MyST Parser options (for Markdown) --------------------------------------
myst_enable_extensions = [
    "amsmath",
    "colon_fence",
    "deflist",
    "dollarmath",
    "html_image",
    "linkify", # Auto-detect links
    "replacements",
    "smartquotes",
    "substitution",
    "tasklist",
]
myst_url_schemes = ["http", "https", "mailto"]

# Ensure that the hero_py package version is correctly picked up
# This might require the package to be installed in editable mode or on PYTHONPATH
# when Sphinx builds. The sys.path modification at the top helps.

# If using sphinx_autodoc_typehints, you might want this:
set_type_checking_flag = True # Default is False
typehints_fully_qualified = False # Set to True for full names if needed
always_document_param_types = True # Default is False
# typehints_document_rtype = True # Default is True

# For ReadTheDocs theme options
# html_theme_options = {
#     'collapse_navigation': False,
#     'sticky_navigation': True,
#     'navigation_depth': 4,
#     'includehidden': True,
#     'titles_only': False
# }

```
This `conf.py` sets up:
*   Project info (name, copyright, version fetched from `hero_py.__version__`).
*   Essential extensions: `autodoc` (for API docs from docstrings), `napoleon` (to understand Google/NumPy style docstrings), `myst_parser` (for Markdown files), `intersphinx` (for linking to other libraries' docs), `viewcode`, `sphinx_autodoc_typehints`.
*   HTML theme set to `sphinx_rtd_theme`.
*   `sys.path` modification to help Sphinx find the `hero_py` package.
*   Basic `autodoc` and `napoleon` settings.

**3. Create `hero_python_port/docs/index.md` (using MyST Parser for Markdown):**
I'm choosing Markdown for the main content pages for ease of writing, enabled by `myst_parser`. Sphinx traditionally uses reStructuredText (`.rst`).
