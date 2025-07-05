from setuptools import setup, find_packages

setup(
    name="pyheRomod",
    version="0.1.0",
    packages=find_packages(),
    author="Jules Agent",
    author_email="jules@example.com",
    description="A Python port of the heRomod R library for health economic modeling.",
    long_description="This library provides tools for Markov models, parameter definition, evaluation, and state expansion, similar to the R heRomod library.",
    url="http://example.com/pyheRomod", # Replace with actual URL if available
    license="MIT", # Assuming MIT, replace if different
    install_requires=[
        "pandas",
        "numpy",
        "scipy"
    ],
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Topic :: Scientific/Engineering",
        "Topic :: Software Development :: Libraries :: Python Modules",
    ],
    python_requires='>=3.10',
)
